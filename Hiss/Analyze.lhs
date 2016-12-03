> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Analyze where
> import Data.List (isPrefixOf)
> import Control.Monad.Except (throwError)
> import Control.Monad.State (get)
> import Control.Monad.Reader (ask)
> import qualified Data.Map.Strict as Map
> import Text.Parsec.Pos (SourcePos, initialPos)
> import Hiss.Data
> import qualified Hiss.Primops as Primops

> ops :: Map.Map String Primop
> ops = Map.fromList [("apply", Applier Primops.apply),
>                     ("call/cc", Applier Primops.callCC),
>                     ("call/vs", Applier Primops.callVs),
>                     ("eval", Evaler eval),
>                     ("values", Impure Primops.values),
>                     ("write", Impure Primops.write),
>                     ("eq?", Impure Primops.eq),
>                     ("eqv?", Impure Primops.eqv),
>                     ("equal?", Impure Primops.equal),
>                     ("add", Impure Primops.add),
>                     ("mul", Impure Primops.mul),
>                     ("sub", Impure Primops.sub),
>                     ("lt", Impure Primops.lt),
>                     ("cons", Impure Primops.cons),
>                     ("pair?", Impure Primops.isPair),
>                     ("car", Impure Primops.car),
>                     ("cdr", Impure Primops.cdr),
>                     ("null?", Impure Primops.isNull),
>                     ("mk-stx", Impure Primops.makeSyntax),
>                     ("stx-e", Impure Primops.syntaxExpr)]

> eval :: EvalerImpl
> eval [stx] = do env <- ask
>                 ast <- liftThrows $ analyze stx
>                 return (ast, env)
> eval _ = flip Argc "%eval" <$> get >>= throwError

> analyze :: SValue -> Either SError AST
> analyze (Syntax (Pair (Syntax (Symbol s) _ _) args) _ pos)
>         | Right (Just res) <- analyzeSf s (ejectList args) pos = return res
>         | Left err <- analyzeSf s (ejectList args) pos = throwError err
> analyze (Syntax (Pair (Syntax (Symbol s) _ _) args) _ pos)
>         | isPrefixOf "##intr#" s =
>     case Map.lookup opname ops of
>       Just op -> Primop pos op <$> mapM analyze (ejectList args)
>       Nothing -> throwError $ NonPrimop pos opname
>     where opname = drop 7 s
> analyze (Syntax (Pair callee args) _ pos) =
>     Call pos <$> (analyze callee) <*> mapM analyze (ejectList args)
> analyze (Syntax (Symbol s) _ pos) = return $ Var pos s
> analyze (Syntax Nil _ pos) = throwError $ NilLiteral pos
> analyze (Syntax v _ pos) = return $ Const pos v

FIXME: error on non-toplevel `define`

> analyzeSf :: String -> [SValue] -> SourcePos -> Either SError (Maybe AST)
> analyzeSf "lambda" [(Syntax formals _ _), body] pos =
>     do (args, restarg) <- analyzeFormals formals
>        Just . Lambda pos args restarg <$> analyze body
> analyzeSf "lambda" [_, _] pos = throwError (Type pos)
> analyzeSf "lambda" _ pos = throwError (Argc pos "lambda")
> analyzeSf "if" [cond, conseq, alt] pos =
>     Just <$> (If pos <$> analyze cond <*> analyze conseq <*> analyze alt)
> analyzeSf "if" _ pos = throwError (Argc pos "if")
> analyzeSf "define" [Syntax (Symbol name) _ _, v] pos =
>     Just . Define pos name <$> analyze v
> analyzeSf "define" [_, _] pos = throwError (Type pos)
> analyzeSf "define" _ pos = throwError (Argc pos "define")
> analyzeSf "set!" [Syntax (Symbol name) _ _, v] pos =
>     Just . Set pos name <$> analyze v
> analyzeSf "set!" [_, _] pos = throwError (Type pos)
> analyzeSf "set!" _ pos = throwError (Argc pos "set!")
> analyzeSf "begin" stmts pos = Just . Begin pos <$> mapM analyze stmts
> analyzeSf "quote" [Syntax datum _ pos] _ = return $ Just (Const pos datum)
> analyzeSf "quote" [_] pos = throwError (Type pos)
> analyzeSf "quote" _ pos = throwError (Argc pos "quote")
> analyzeSf "syntax" [stx @ (Syntax _ _ pos)] _ = return $ Just (Const pos stx)
> analyzeSf "syntax" [_] pos = throwError (Type pos)
> analyzeSf "syntax" _ pos = throwError (Argc pos "syntax")
> analyzeSf _ _ _ = return Nothing

> analyzeFormals :: SValue -> Either SError ([String], Maybe String)
> analyzeFormals (Pair (Syntax (Symbol f) _ _) rfs @ (Pair _ _)) =
>     do (fs, rf) <- analyzeFormals rfs
>        return (f:fs, rf)
> analyzeFormals (Pair (Syntax (Symbol f) _ _) Nil) =
>     return ([f], Nothing)
> analyzeFormals (Pair (Syntax (Symbol f) _ _) (Syntax (Symbol rf) _ _)) =
>     return ([f], Just rf)
> analyzeFormals Nil = return ([], Nothing)
> analyzeFormals (Symbol rf) = return ([], Just rf)
> analyzeFormals formals @ (Syntax _ _ pos) =
>     throwError $ InvalidFormals pos formals
> analyzeFormals formals = throwError $ InvalidFormals (initialPos "???") formals
