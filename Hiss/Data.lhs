> module Hiss.Data where
> import Control.Monad.Except (ExceptT)
> import Control.Monad.State (StateT)
> import Control.Monad.Identity (Identity)
> import qualified Data.Map.Strict as Map
> import Data.Array

> type EvalState t = StateT Store (ExceptT SError Identity) t

= Value Representation

> type BuiltinImpl = [SValue] -> EvalState SValue

> data SValue = Symbol String
>             | Bool Bool
>             | Fixnum Int
>             | Pair SValue SValue
>             | Closure [String] (Maybe String) AST Env
>             | Builtin BuiltinImpl
>             | Nil
>             | Continuation Cont
>             | CallCC
>             | Unspecified
>             | Unbound

> instance Show SValue where
>   show (Symbol cs) = cs
>   show (Bool True) = "#t"
>   show (Bool False) = "#f"
>   show (Fixnum n) = show n
>   show (Pair x xs) = '(' : show x ++ showElems xs
>       where showElems (Pair y ys) = ' ' : show y ++ showElems ys
>             showElems Nil = ")"
>             showElems y = " . " ++ show y ++ ")"
>   show (Closure _ _ _ _) = "#<lambda>"
>   show (Builtin _) = "#<lambda>"
>   show Nil = "()"
>   show (Continuation _) = "#<lambda>"
>   show CallCC = "#<lambda>"
>   show Unspecified = "#<unspecified>"
>   show Unbound = "#<unbound>"

> injectList :: [SValue] -> SValue
> injectList (v:vs) = Pair v (injectList vs)
> injectList [] = Nil

> ejectList :: SValue -> [SValue]
> ejectList (Pair x xs) = x : ejectList xs
> ejectList Nil = []

= Abstract Syntax Tree and Continuations

> data AST = Lambda [String] (Maybe String) AST
>          | Call AST [AST]
>          | If AST AST AST
>          | Begin [AST]
>          | Set String AST
>          | Var String
>          | Const SValue
>          deriving (Show)

> data Cont = Fn Cont Env [AST]
>           | Arg Cont Env SValue [SValue] [AST]
>           | Cond Cont Env AST AST
>           | SetName Cont Env String
>           | Began Cont Env [AST]
>           | Halt

= Environment

> type Address = Int
> type Env = Map.Map String Address

> emptyEnv :: Env
> emptyEnv = Map.empty

= Store

> data Store = Store (Array Address SValue) Address

> emptyStore :: Store
> emptyStore = Store (listArray (0, 999999) (replicate 1000000 Unbound)) 0

> deref :: Address -> Store -> SValue
> deref a (Store vs _) = vs ! a

> alloc :: Store -> SValue -> (Address, Store) -- TODO: GC
> alloc (Store vs a) v = (a, Store (vs // [(a, v)]) (a + 1))

> set :: Address -> SValue -> Store -> Store
> set a v (Store vs n) = Store (vs // [(a, v)]) n

> def :: Env -> Store -> String -> SValue -> (Env, Store)
> def e s n v = (Map.insert n a e, s')
>     where (a, s') = alloc s v

= Errors

> data SError = Nonbound String
>             | NonLambda SValue
>             | Argc
>             | Type
>               deriving (Show)
