> module Hiss.Analyze where
> import Data.List (isPrefixOf)
> import Hiss.Data
> import qualified Hiss.Builtins as Builtins

> analyze :: SValue -> AST
> analyze (Pair (Symbol s) args) | isPrefixOf "##sf#" s =
>     analyzeSf (drop 5 s) (ejectList args)
> analyze (Pair (Symbol s) args) | isPrefixOf "##intr#" s =
>     Primop (lookupPrimop $ drop 7 s) (map analyze $ ejectList args)
> analyze (Pair callee args) =
>     Call (analyze callee) (map analyze $ ejectList args)
> analyze (Symbol s) = Var s
> analyze v = Const v

> analyzeSf :: String -> [SValue] -> AST
> analyzeSf "lambda" [formals, body] = Lambda args restarg $ analyze body
>     where (args, restarg) = analyzeFormals formals
>           analyzeFormals (Pair (Symbol f) fs) = (f:formals, restFormal)
>               where (formals, restFormal) = analyzeFormals fs
>           analyzeFormals (Symbol rf) = ([], Just rf)
>           analyzeFormals Nil = ([], Nothing)
> analyzeSf "if" [cond, conseq, alt] = If (analyze cond)
>                                         (analyze conseq)
>                                         (analyze alt)
> analyzeSf "set!" [Symbol name, v] = Set name (analyze v)
> analyzeSf "begin" stmts = Begin $ map analyze stmts
> analyzeSf "quote" [datum] = Const datum

> lookupPrimop :: String -> Primop
> lookupPrimop "values"    = Pure Builtins.values
> lookupPrimop "defglobal" = Impure Builtins.defglobal
> lookupPrimop "write"     = Impure Builtins.write
> lookupPrimop "add"       = Pure Builtins.add
> lookupPrimop "mul"       = Pure Builtins.mul
> lookupPrimop "sub"       = Pure Builtins.sub
> lookupPrimop "lt"        = Pure Builtins.lt
