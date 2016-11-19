> module Hiss.Analyze where
> import Hiss.Data

> analyze :: SValue -> AST
> analyze (Pair (Symbol "set!")
>               (Pair (Symbol name)
>                     (Pair val Nil))) = Set name $ analyze val
> analyze (Symbol s) = Var s
> analyze v = Const v
