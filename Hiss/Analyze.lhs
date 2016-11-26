> module Hiss.Analyze where
> import Data.List (isPrefixOf)
> import Text.Parsec.Pos (SourcePos)
> import Hiss.Data
> import qualified Hiss.Builtins as Builtins

> analyze :: SValue -> AST
> analyze (Syntax (Pair (Syntax (Symbol s) _ _) args) _ pos)
>         | isPrefixOf "##sf#" s = analyzeSf (drop 5 s) (ejectList args) pos
> analyze (Syntax (Pair (Syntax (Symbol s) _ _) args) _ pos)
>         | isPrefixOf "##intr#" s =
>     Primop pos (lookupPrimop $ drop 7 s) (map analyze $ ejectList args)
> analyze (Syntax (Pair callee args) _ pos) =
>     Call pos (analyze callee) (map analyze $ ejectList args)
> analyze (Syntax (Symbol s) _ pos) = Var pos s
> analyze (Syntax v _ pos) = Const pos v

> analyzeSf :: String -> [SValue] -> SourcePos -> AST
> analyzeSf "lambda" [(Syntax formals _ _), body] pos =
>     Lambda pos args restarg $ analyze body
>     where (args, restarg) = analyzeFormals formals
>           analyzeFormals (Pair (Syntax (Symbol f) _ _) fs) = (f:fs', rf)
>               where (fs', rf) = analyzeFormals fs
>           analyzeFormals (Syntax (Symbol rf) _ _) = ([], Just rf)
>           analyzeFormals Nil = ([], Nothing)
> analyzeSf "if" [cond, conseq, alt] pos = If pos (analyze cond)
>                                             (analyze conseq)
>                                             (analyze alt)
> analyzeSf "set!" [Syntax (Symbol name) _ _, v] pos = Set pos name (analyze v)
> analyzeSf "begin" stmts pos = Begin pos $ map analyze stmts
> analyzeSf "quote" [Syntax datum _ pos] _ = Const pos datum

> lookupPrimop :: String -> Primop
> lookupPrimop "apply"     = Applier Builtins.apply
> lookupPrimop "call/cc"   = Applier Builtins.callCC
> lookupPrimop "call/vs"   = Applier Builtins.callVs
> lookupPrimop "values"    = Pure Builtins.values
> lookupPrimop "defglobal" = Impure Builtins.defglobal
> lookupPrimop "write"     = Impure Builtins.write
> lookupPrimop "add"       = Pure Builtins.add
> lookupPrimop "mul"       = Pure Builtins.mul
> lookupPrimop "sub"       = Pure Builtins.sub
> lookupPrimop "lt"        = Pure Builtins.lt
