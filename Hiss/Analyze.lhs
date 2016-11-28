> module Hiss.Analyze where
> import Data.List (isPrefixOf)
> import Data.Maybe (fromJust)
> import Control.Monad.Except (throwError)
> import qualified Data.Map.Strict as Map
> import Text.Parsec.Pos (SourcePos)
> import Hiss.Data
> import qualified Hiss.Primops as Primops

FIXME: get rid of fromJust

> analyze :: SValue -> Either SError AST
> analyze (Syntax (Pair (Syntax (Symbol s) _ _) args) _ pos)
>         | Right (Just res) <- analyzeSf s (ejectList args) pos = return res
>         | Left err <- analyzeSf s (ejectList args) pos = throwError err
> analyze (Syntax (Pair (Syntax (Symbol s) _ _) args) _ pos)
>         | isPrefixOf "##intr#" s =
>     Primop pos (fromJust $ Map.lookup (drop 7 s) Primops.ops)
>     <$> mapM analyze (ejectList args)
> analyze (Syntax (Pair callee args) _ pos) =
>     Call pos <$> (analyze callee) <*> mapM analyze (ejectList args)
> analyze (Syntax (Symbol s) _ pos) = return $ Var pos s
> analyze (Syntax Nil _ pos) = throwError $ NilLiteral pos
> analyze (Syntax v _ pos) = return $ Const pos v

FIXME: error on non-toplevel `define`

> analyzeSf :: String -> [SValue] -> SourcePos -> Either SError (Maybe AST)
> analyzeSf "lambda" [(Syntax formals _ _), body] pos =
>     Just . Lambda pos args restarg <$> analyze body
>     where (args, restarg) = analyzeFormals formals
>           analyzeFormals (Pair (Syntax (Symbol f) _ _) fs) = (f:fs', rf)
>               where (fs', rf) = analyzeFormals fs
>           analyzeFormals (Syntax (Symbol rf) _ _) = ([], Just rf)
>           analyzeFormals Nil = ([], Nothing)
> analyzeSf "lambda" [_, _] pos = throwError (Type pos)
> analyzeSf "lambda" _ pos = throwError (Argc pos)
> analyzeSf "if" [cond, conseq, alt] pos =
>     Just <$> (If pos <$> analyze cond <*> analyze conseq <*> analyze alt)
> analyzeSf "if" _ pos = throwError (Argc pos)
> analyzeSf "set!" [Syntax (Symbol name) _ _, v] pos =
>     Just . Set pos name <$> analyze v
> analyzeSf "set!" [_, _] pos = throwError (Type pos)
> analyzeSf "set!" _ pos = throwError (Argc pos)
> analyzeSf "begin" stmts pos = Just . Begin pos <$> mapM analyze stmts
> analyzeSf "quote" [Syntax datum _ pos] _ = return $ Just (Const pos datum)
> analyzeSf "quote" [_] pos = throwError (Type pos)
> analyzeSf "quote" _ pos = throwError (Argc pos)
> analyzeSf "define" [Syntax name @ (Symbol _) _ npos, v] pos =
>     do vast <- analyze v
>        return $ Just
>            (Primop pos (Impure Primops.defglobal) [Const npos name, vast])
> analyzeSf "define" [_, _] pos = throwError (Type pos)
> analyzeSf "define" _ pos = throwError (Argc pos)
> analyzeSf _ _ _ = return Nothing
