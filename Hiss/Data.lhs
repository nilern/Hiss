> module Hiss.Data where

= Value Representation

> data SValue = Symbol String
>             | Bool Bool
>             | Pair SValue SValue
>             | Nil
