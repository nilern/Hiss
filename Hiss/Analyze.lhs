> module Hiss.Analyze where
> import Data.List (isPrefixOf)
> import Data.Maybe (fromJust)
> import qualified Data.Map.Strict as Map
> import Text.Parsec.Pos (SourcePos)
> import Hiss.Data
> import qualified Hiss.Primops as Primops

> analyze :: SValue -> AST -- FIXME: error handling
> analyze (Syntax (Pair (Syntax (Symbol s) _ _) args) _ pos)
>         | Just res <- analyzeSf s (ejectList args) pos = res
> analyze (Syntax (Pair (Syntax (Symbol s) _ _) args) _ pos)
>         | isPrefixOf "##intr#" s =
>     Primop pos (fromJust $ Map.lookup (drop 7 s) Primops.ops)
>                (map analyze $ ejectList args)
> analyze (Syntax (Pair callee args) _ pos) =
>     Call pos (analyze callee) (map analyze $ ejectList args)
> analyze (Syntax (Symbol s) _ pos) = Var pos s
> analyze (Syntax v _ pos) = Const pos v

FIXME: error handling, among other things warn about non-toplevel `define`

> analyzeSf :: String -> [SValue] -> SourcePos -> Maybe AST
> analyzeSf "lambda" [(Syntax formals _ _), body] pos =
>     return $ Lambda pos args restarg $ analyze body
>     where (args, restarg) = analyzeFormals formals
>           analyzeFormals (Pair (Syntax (Symbol f) _ _) fs) = (f:fs', rf)
>               where (fs', rf) = analyzeFormals fs
>           analyzeFormals (Syntax (Symbol rf) _ _) = ([], Just rf)
>           analyzeFormals Nil = ([], Nothing)
> analyzeSf "if" [cond, conseq, alt] pos = return $ If pos (analyze cond)
>                                                          (analyze conseq)
>                                                          (analyze alt)
> analyzeSf "set!" [Syntax (Symbol name) _ _, v] pos =
>     return $ Set pos name (analyze v)
> analyzeSf "begin" stmts pos = return $ Begin pos $ map analyze stmts
> analyzeSf "quote" [Syntax datum _ pos] _ = return $ Const pos datum
> analyzeSf "define" [Syntax name @ (Symbol _) _ npos, v] pos =
>     return $ Primop pos (Impure Primops.defglobal)
>                     [Const npos name, analyze v]
> analyzeSf _ _ _ = Nothing
