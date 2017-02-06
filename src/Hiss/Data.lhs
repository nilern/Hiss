= Value Representation

> {-# LANGUAGE BangPatterns, RankNTypes, FlexibleContexts, MagicHash #-}

> module Hiss.Data where
> import Prelude hiding (lookup)
> import System.IO (Handle)
> import GHC.Prim (reallyUnsafePtrEquality#)
> import Control.Eff
> import Control.Eff.Exception
> import Control.Eff.Lift
> import Control.Eff.State.Lazy
> import Control.Eff.Reader.Lazy
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set
> import qualified Data.HashTable.IO as H
> import Text.Parsec (ParseError)
> import Text.Parsec.Pos (SourcePos, initialPos)

Here we have some type definitions for primop-implementing functions. They
differ in the effects they perform and whether they need direct access to the
continuation value.

> type ExcImpl =
>     forall r . (Member (Exc SError) r, Member (State SourcePos) r)
>              => [SValue] -> Eff r [SValue]
> type PrimopImpl =
>     forall r . (Member (State SourcePos) r, Member (Exc SError) r,
>                 SetMember Lift (Lift IO) r)
>              => [SValue] -> Eff r [SValue]
> type ApplierImpl =
>     forall r . (Member (State SourcePos) r, Member (Exc SError) r)
>              => Cont -> [SValue] -> Eff r (Cont, SValue, [SValue])
> type EvalerImpl =
>     forall r . (Member (State SourcePos) r, Member (Reader Env) r,
>                 Member (Exc SError) r)
>              => [SValue] -> Eff r (AST, Env)

The `Context` will be used to implement hygienic macros as outlined in the
[Binding as Sets of Scopes](https://www.cs.utah.edu/plt/scope-sets/) paper.

> type Context = Map.Map Int (Set.Set SValue)

`SValue` is the Haskell type corresponding to all first-class Scheme values.

> data SValue = Symbol String
>             | String String
>             | Fixnum Int
>             | Bool Bool
>             | Pair SValue SValue
>             | Nil
>             | Syntax SValue Context SourcePos
>             | Closure [String] (Maybe String) AST Env
>             | Continuation Cont
>             | Values
>             | Port Handle
>             | Unbound
>             | Unspecified

> instance Eq SValue where
>   (Symbol a) == (Symbol b) = a == b
>   (String a) == (String b) = a == b
>   (Fixnum a) == (Fixnum b) = a == b
>   (Bool a) == (Bool b) = a == b
>   (Pair a b) == (Pair c d) = a == c && b == d
>   Nil == Nil = True
>   (Syntax a aCtx aPos) == (Syntax b bCtx bPos) =
>       a == b && aCtx == bCtx && aPos == bPos
>   (!a @ (Closure _ _ _ _)) == (!b @ (Closure _ _ _ _)) =
>       case reallyUnsafePtrEquality# a b of
>         1# -> True
>         _ -> False
>   (!k @ (Continuation _)) == (!l @ (Continuation _)) =
>       case reallyUnsafePtrEquality# k l of
>         1# -> True
>         _ -> False
>   Values == Values = True
>   (Port h1) == (Port h2) = h1 == h2
>   Unbound == Unbound = True
>   Unspecified == Unspecified = True
>   _ == _ = False

> instance Show SValue where
>   show (Symbol cs) = cs
>   show (String s) = '"' : s ++ "\""
>   show (Fixnum n) = show n
>   show (Bool True) = "#t"
>   show (Bool False) = "#f"
>   show (Pair x xs) = '(' : show x ++ showElems xs
>       where showElems (Pair y ys) = ' ' : show y ++ showElems ys
>             showElems Nil = ")"
>             showElems y = " . " ++ show y ++ ")"
>   show Nil = "()"
>   show (Syntax v _ _) = "#<syntax " ++ show v ++ ">"
>   show (Closure _ _ _ _) = "#<lambda>"
>   show (Continuation _) = "#<lambda>"
>   show Values = "#<lambda>"
>   show (Port _) = "#<port>"
>   show Unbound = "#<unbound>"
>   show Unspecified = "#<unspecified>"

Scheme lists can be dotted while Haskell lists cannot. Since Scheme lists are
represented differently we need `injectList` to convert Haskell lists to Scheme
lists and `ejectList` to do the opposite.

> injectList :: [SValue] -> SValue
> injectList (v:vs) = Pair v (injectList vs)
> injectList [] = Nil

> -- FIXME: This should be a complete function and return an error monad.
> ejectList :: SValue -> [SValue]
> ejectList (Pair x xs) = x : ejectList xs
> ejectList Nil = []

= Abstract Syntax Tree and Continuations

The `Positioned` type class is used to extract source positions from both AST:s
and continuations.

> class Positioned a where
>   positionOf :: a -> SourcePos

To make the interpreter clearer and faster we translate S-expressions (actually,
syntax objects) into an explicit abstract syntax tree. The position information
from syntax objects is also folded into the tree to provide tolerable error
messages.

> data AST = Lambda SourcePos [String] (Maybe String) AST
>          | Call SourcePos AST [AST]
>          | Primop SourcePos Primop [AST]
>          | If SourcePos AST AST AST
>          | Begin SourcePos [AST]
>          | Define SourcePos String AST
>          | Set SourcePos String AST
>          | Var SourcePos String
>          | Const SourcePos SValue

> instance Positioned AST where
>   positionOf (Lambda pos _ _ _) = pos
>   positionOf (Call pos _ _) = pos
>   positionOf (Primop pos _ _) = pos
>   positionOf (If pos _ _ _) = pos
>   positionOf (Begin pos _) = pos
>   positionOf (Define pos _ _) = pos
>   positionOf (Set pos _ _) = pos
>   positionOf (Var pos _) = pos
>   positionOf (Const pos _) = pos

Primops represent useful primitive operations. The implementations can be found
in `Hiss.Primops`.

> data Primop = Purish ExcImpl
>             | Impure PrimopImpl
>             | Applier ApplierImpl
>             | Evaler EvalerImpl

Continuations represent the remainder of the computation. They form a linked
stack.

> data Cont = Fn SourcePos Cont Env [AST]
>           | Arg SourcePos Cont Env SValue [SValue] [AST]
>           | PrimArg SourcePos Cont Env Primop [SValue] [AST]
>           | AppVs SourcePos Cont SValue
>           | Cond SourcePos Cont Env AST AST
>           | Began SourcePos Cont Env [AST]
>           | DefineName SourcePos Cont Env String
>           | SetName SourcePos Cont Env String
>           | Halt SourcePos

> instance Positioned Cont where
>   positionOf (Fn pos _ _ _) = pos
>   positionOf (Arg pos _ _ _ _ _) = pos
>   positionOf (PrimArg pos _ _ _ _ _) = pos
>   positionOf (AppVs pos _ _) = pos
>   positionOf (Cond pos _ _ _ _) = pos
>   positionOf (Began pos _ _ _) = pos
>   positionOf (DefineName pos _ _ _) = pos
>   positionOf (SetName pos _ _ _) = pos
>   positionOf (Halt pos) = pos

= Environment

The environment maps variable names to the corresponding values. To implement
lexical scope environments are represented as linked lists of frames. Since
Scheme has `set!` and a top level that can be incrementally extended we use hash
tables to store the bindings instead of alists, `Data.Map` or similar persistent
associative structures.

> data Env = Lexical Env (H.BasicHashTable String SValue)
>          | Global (H.BasicHashTable String SValue)

> emptyEnv :: IO Env
> emptyEnv = Global <$> H.new

As usual, `lookup` reads the value of a variable. Its type is a bit involved but
all it means is that the lookup operation involves mutable state
(the environment) and could also fail, in which case we want to be able to
provide the source position in the error message.

> lookup :: (Member (Exc SError) r, Member (State SourcePos) r,
>            SetMember Lift (Lift IO) r)
>        => Env -> String -> Eff r SValue
> lookup (Lexical parent kvs) name =
>     do ov <- lift $ H.lookup kvs name
>        case ov of
>          Just v -> return v
>          Nothing -> lookup parent name
> lookup (Global kvs) name =
>     do ov <- lift $ H.lookup kvs name
>        case ov of
>          Just v -> return v
>          Nothing -> flip Nonbound name <$> get >>= throwExc

`pushFrame` adds a new environment frame that 'inherits' from the provided one.
It is used when entering a closure.

> pushFrame :: Env -> [(String, SValue)] -> IO Env
> pushFrame parent kvs = Lexical parent <$> H.fromList kvs

`define` implements toplevel Scheme `define`s and `set` implements `set!`. They
are effectful in precisely the same way as `lookup`.

> define :: (Member (State SourcePos) r, Member (Exc SError) r,
>            SetMember Lift (Lift IO) r)
>        => Env -> String -> SValue -> Eff r ()
> define (Lexical _ _) name _ = flip LexicalDefine name <$> get >>= throwExc
> define (Global kvs) name v = lift $ H.insert kvs name v

> set :: (Member (State SourcePos) r, Member (Exc SError) r,
>         SetMember Lift (Lift IO) r)
>     => Env -> String -> SValue -> Eff r ()
> set (Lexical parent kvs) name v =
>     do ov <- lift $ H.lookup kvs name
>        case ov of
>          Just _ -> lift $ H.insert kvs name v
>          Nothing -> set parent name v
> set (Global kvs) name v =
>     do ov <- lift $ H.lookup kvs name
>        case ov of
>          Just _ -> lift $ H.insert kvs name v
>          Nothing -> flip Nonbound name <$> get >>= throwExc

= Errors

`SError` represents all the various errors that can occur in the interpreter.

> data SError = Nonbound SourcePos String
>             | LexicalDefine SourcePos String
>             | NonLambda SourcePos SValue
>             | NonPrimop SourcePos String
>             | Argc SourcePos String
>             | InvalidFormals SourcePos SValue
>             | Type SourcePos String SValue
>             | Formals SourcePos SValue
>             | NilLiteral SourcePos
>             | NonError SourcePos
>             | ParseError SourcePos ParseError
>               deriving (Show)

HACK: This `Monoid` instance and the `NonError` variant it uses are silly and
exist to please some type signature somewhere. There should be a better way...

> instance Monoid SError where
>   mempty = NonError $ initialPos "__no-file__"
>   mappend _ e = e
