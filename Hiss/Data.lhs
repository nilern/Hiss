> module Hiss.Data where
> import Data.Map.Strict as Map

= Value Representation

> data SValue = Symbol String
>             | Bool Bool
>             | Fixnum Int
>             | Pair SValue SValue
>             | Nil

> instance Show SValue where
>   show (Symbol cs) = cs
>   show (Bool True) = "#t"
>   show (Bool False) = "#f"
>   show (Fixnum n) = show n
>   show (Pair x xs) = '(' : show x ++ showElems xs
>       where showElems (Pair x xs) = ' ' : show x ++ showElems xs
>             showElems Nil = ")"
>             showElems x = " . " ++ show x ++ ")"
>   show Nil = "()"

= Abstract Syntax Tree

> data AST = Lambda [String] (Maybe String) Env AST
>          | Call AST [AST]
>          | If AST AST AST
>          | Set String AST
>          | Var String
>          | Const SValue
>          deriving (Show)

= Environment

> type Env = Map.Map String SValue

> emptyEnv :: Env
> emptyEnv = Map.empty
