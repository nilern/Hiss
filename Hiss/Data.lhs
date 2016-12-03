> {-# LANGUAGE BangPatterns, RankNTypes, FlexibleContexts #-}

> module Hiss.Data where
> import Prelude hiding (lookup)
> import System.IO (Handle)
> import System.IO.Unsafe (unsafePerformIO)
> import System.Mem.StableName
> -- import Control.Monad.Except (ExceptT, throwError, liftIO)
> -- import Control.Monad.State (StateT, get)
> -- import Control.Monad.Reader (ReaderT)
> import Control.Eff
> import Control.Eff.Exception
> import Control.Eff.Lift
> import Control.Eff.State.Lazy
> import Control.Eff.Reader.Lazy
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set
> import qualified Data.HashTable.IO as H
> import Text.Parsec.Pos (SourcePos, initialPos)

= Value Representation

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

> type Context = Map.Map Int (Set.Set SValue)

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
>       unsafePerformIO $ (==) <$> makeStableName a <*> makeStableName b
>   (!k @ (Continuation _)) == (!l @ (Continuation _)) =
>       unsafePerformIO $ (==) <$> makeStableName k <*> makeStableName l
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

> injectList :: [SValue] -> SValue
> injectList (v:vs) = Pair v (injectList vs)
> injectList [] = Nil

> ejectList :: SValue -> [SValue]
> ejectList (Pair x xs) = x : ejectList xs
> ejectList Nil = []

= Abstract Syntax Tree and Continuations

> class Positioned a where
>   positionOf :: a -> SourcePos

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

> data Primop = Purish ExcImpl
>             | Impure PrimopImpl
>             | Applier ApplierImpl
>             | Evaler EvalerImpl

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

> data Env = Lexical Env (H.BasicHashTable String SValue)
>          | Global (H.BasicHashTable String SValue)

> emptyEnv :: IO Env
> emptyEnv = Global <$> H.new

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

> pushFrame :: Env -> [(String, SValue)] -> IO Env
> pushFrame parent kvs = Lexical parent <$> H.fromList kvs

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

> data SError = Nonbound SourcePos String
>             | LexicalDefine SourcePos String
>             | NonLambda SourcePos SValue
>             | NonPrimop SourcePos String
>             | Argc SourcePos String
>             | InvalidFormals SourcePos SValue
>             | Type SourcePos
>             | Formals SourcePos SValue
>             | NilLiteral SourcePos
>             | NonError SourcePos
>               deriving (Show)

> instance Monoid SError where
>   mempty = NonError $ initialPos "__no-file__"
>   mappend _ e = e
