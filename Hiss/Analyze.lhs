> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Analyze where
> import Control.Eff hiding (Impure)
> import Control.Eff.Exception
> import Control.Eff.State.Lazy
> import Control.Eff.Reader.Lazy
> import Data.List (isPrefixOf)
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
>                 ast <- analyze stx
>                 return (ast, env)
> eval _ = flip Argc "%eval" <$> get >>= throwExc

> analyze :: (Member (Exc SError) r) => SValue -> Eff r AST
> analyze (Syntax (Pair callee @ (Syntax (Symbol s) _ _) args) _ pos) =
>     do oast <- analyzeSf s (ejectList args) pos
>        case oast of
>          Just ast -> return ast
>          Nothing ->
>              if isPrefixOf "##intr#" s
>              then let opname = drop 7 s in
>                   case Map.lookup opname ops of
>                     Just op -> Primop pos op <$> mapM analyze (ejectList args)
>                     Nothing -> throwExc $ NonPrimop pos opname
>              else Call pos <$> (analyze callee) <*> mapM analyze (ejectList args)
> analyze (Syntax (Pair callee args) _ pos) =
>     Call pos <$> (analyze callee) <*> mapM analyze (ejectList args)
> analyze (Syntax (Symbol s) _ pos) = return $ Var pos s
> analyze (Syntax Nil _ pos) = throwExc $ NilLiteral pos
> analyze (Syntax v _ pos) = return $ Const pos v

FIXME: error on non-toplevel `define`

> analyzeSf :: (Member (Exc SError) r)
>           => String -> [SValue] -> SourcePos -> Eff r (Maybe AST)
> analyzeSf "lambda" [(Syntax formals _ _), body] pos =
>     do (args, restarg) <- analyzeFormals formals
>        Just . Lambda pos args restarg <$> analyze body
> analyzeSf "lambda" [_, _] pos = throwExc (Type pos)
> analyzeSf "lambda" _ pos = throwExc (Argc pos "lambda")
> analyzeSf "if" [cond, conseq, alt] pos =
>     Just <$> (If pos <$> analyze cond <*> analyze conseq <*> analyze alt)
> analyzeSf "if" _ pos = throwExc (Argc pos "if")
> analyzeSf "define" [Syntax (Symbol name) _ _, v] pos =
>     Just . Define pos name <$> analyze v
> analyzeSf "define" [_, _] pos = throwExc (Type pos)
> analyzeSf "define" _ pos = throwExc (Argc pos "define")
> analyzeSf "set!" [Syntax (Symbol name) _ _, v] pos =
>     Just . Set pos name <$> analyze v
> analyzeSf "set!" [_, _] pos = throwExc (Type pos)
> analyzeSf "set!" _ pos = throwExc (Argc pos "set!")
> analyzeSf "begin" stmts pos = Just . Begin pos <$> mapM analyze stmts
> analyzeSf "quote" [Syntax datum _ pos] _ = return $ Just (Const pos datum)
> analyzeSf "quote" [_] pos = throwExc (Type pos)
> analyzeSf "quote" _ pos = throwExc (Argc pos "quote")
> analyzeSf "syntax" [stx @ (Syntax _ _ pos)] _ = return $ Just (Const pos stx)
> analyzeSf "syntax" [_] pos = throwExc (Type pos)
> analyzeSf "syntax" _ pos = throwExc (Argc pos "syntax")
> analyzeSf _ _ _ = return Nothing

> analyzeFormals :: (Member (Exc SError) r)
>                => SValue -> Eff r ([String], Maybe String)
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
>     throwExc $ InvalidFormals pos formals
> analyzeFormals formals = throwExc $ InvalidFormals (initialPos "???") formals
