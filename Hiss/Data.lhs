> module Hiss.Data where
> import Data.Map.Strict as Map

= Value Representation

> data SValue = Symbol String
>             | Bool Bool
>             | Fixnum Int
>             | Pair SValue SValue
>             | Closure [String] (Maybe String) AST Env
>             | Nil
>             | Unspecified

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

= Abstract Syntax Tree and Continuations

> data AST = Lambda [String] (Maybe String) AST
>          | Call AST [AST]
>          | If AST AST AST
>          | Begin [AST]
>          | Var String
>          | Const SValue
>          deriving (Show)

> data Cont = Fn Cont [AST]
>           | Arg Cont SValue [SValue] [AST]
>           | Cond Cont AST AST
>           | Began Cont [AST]
>           | Halt

= Environment

> type Env = Map.Map String SValue

> emptyEnv :: Env
> emptyEnv = Map.empty

= Errors

> data SError = Unbound String
>             | NonLambda SValue
>             | Argc
>               deriving (Show)
