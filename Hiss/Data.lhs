> module Hiss.Data where
> import qualified Data.Map.Strict as Map
> import Data.Array

= Value Representation

> data SValue = Symbol String
>             | Bool Bool
>             | Fixnum Int
>             | Pair SValue SValue
>             | Closure [String] (Maybe String) AST Env
>             | Nil
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
>   show (Closure _ _ _ _) = "#<lambda closure>"
>   show Nil = "()"
>   show Unspecified = "#<unspecified>"
>   show Unbound = "#<unbound>"

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

> initStore :: Store
> initStore = Store (listArray (0, 4999) (replicate 5000 Unbound)) 0

> deref :: Address -> Store -> SValue
> deref a (Store vs _) = vs ! a

= Errors

> data SError = Nonbound String
>             | NonLambda SValue
>             | Argc
>               deriving (Show)
