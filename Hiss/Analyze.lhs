> module Hiss.Analyze where
> import Hiss.Data

> analyze :: SValue -> AST
> analyze (Pair (Symbol "lambda")
>               (Pair formals body)) =
>     Lambda args restarg $ analyze (Pair (Symbol "begin") body)
>     where (args, restarg) = analyzeFormals formals
> analyze (Pair (Symbol "if")
>               (Pair cond
>                     (Pair conseq
>                           (Pair alt Nil)))) = If (analyze cond)
>                                                  (analyze conseq)
>                                                  (analyze alt)
> analyze (Pair (Symbol "define")
>               (Pair (Symbol name)
>                     (Pair v Nil))) = Call (Var "define")
>                                           [Const $ Symbol name, analyze v]
> analyze (Pair (Symbol "set!")
>               (Pair (Symbol name)
>                     (Pair v Nil))) = Set name (analyze v)
> analyze (Pair (Symbol "begin") stmts) = Begin $ map analyze $ ejectList stmts
> analyze (Pair (Symbol "quote")
>               (Pair datum Nil)) = Const datum
> analyze (Pair callee args) =
>     Call (analyze callee) $ map analyze $ ejectList args
> analyze (Symbol s) = Var s
> analyze v = Const v

> analyzeFormals :: SValue -> ([String], Maybe String)
> analyzeFormals (Pair (Symbol f) fs) = (f:formals, restFormal)
>     where (formals, restFormal) = analyzeFormals fs
> analyzeFormals (Symbol rf) = ([], Just rf)
> analyzeFormals Nil = ([], Nothing)
