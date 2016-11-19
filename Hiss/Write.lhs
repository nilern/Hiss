> module Hiss.Write where
> import Hiss.Data

= Write

> instance Show SValue where
>   show (Symbol cs) = cs
>   show (Bool True) = "#t"
>   show (Bool False) = "#f"
>   show (Pair x xs) = '(' : show x ++ showElems xs
>       where showElems (Pair x xs) = ' ' : show x ++ showElems xs
>             showElems Nil = ")"
>             showElems x = " . " ++ show x ++ ")"
>   show Nil = "()"
