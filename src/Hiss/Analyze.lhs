
= The Analyzer

The analyzer transforms syntax objects into abstract syntax trees. Not having to
deal with the complex and possibly erroneous structure of syntax objects greatly
simplifies the interpreter. This module could also be regarded as a very simple
compiler. Doing all this analysis work upfront and producing a more compact
representation of the program should also speed up the interpreter somewhat.

> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Analyze (analyze) where
> import System.IO (IOMode(ReadMode, WriteMode))
> import Control.Eff hiding (Impure)
> import Control.Eff.Exception
> import Control.Eff.State.Lazy
> import Control.Eff.Reader.Lazy
> import Data.List (isPrefixOf)
> import qualified Data.Map.Strict as Map
> import Text.Parsec.Pos (SourcePos, initialPos)
> import Hiss.Data
> import qualified Hiss.Primops as Primops

`analyze` takes a syntax objects and returns the corresponding AST or an error
using the `Exception` effect.

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

`analyzeSf` analyzes the special forms. It is straightforward but tedious.

> -- FIXME: signal error on non-toplevel `define`
> analyzeSf :: (Member (Exc SError) r)
>           => String -> [SValue] -> SourcePos -> Eff r (Maybe AST)
> analyzeSf "lambda" [(Syntax formals _ _), body] pos =
>     do (args, restarg) <- analyzeFormals formals
>        Just . Lambda pos args restarg <$> analyze body
> analyzeSf "lambda" [s, _] pos = throwExc (Type pos "syntax" s)
> analyzeSf "lambda" _ pos = throwExc (Argc pos "lambda")
> analyzeSf "if" [cond, conseq, alt] pos =
>     Just <$> (If pos <$> analyze cond <*> analyze conseq <*> analyze alt)
> analyzeSf "if" _ pos = throwExc (Argc pos "if")
> analyzeSf "define" [Syntax (Symbol name) _ _, v] pos =
>     Just . Define pos name <$> analyze v
> analyzeSf "define" [s, _] pos = throwExc (Type pos "syntax" s)
> analyzeSf "define" _ pos = throwExc (Argc pos "define")
> analyzeSf "set!" [Syntax (Symbol name) _ _, v] pos =
>     Just . Set pos name <$> analyze v
> analyzeSf "set!" [s, _] pos = throwExc (Type pos "syntax" s)
> analyzeSf "set!" _ pos = throwExc (Argc pos "set!")
> analyzeSf "begin" stmts pos = Just . Begin pos <$> mapM analyze stmts
> analyzeSf "quote" [Syntax datum _ pos] _ = return $ Just (Const pos datum)
> analyzeSf "quote" [s] pos = throwExc (Type pos "syntax" s)
> analyzeSf "quote" _ pos = throwExc (Argc pos "quote")
> analyzeSf "syntax" [stx @ (Syntax _ _ pos)] _ = return $ Just (Const pos stx)
> analyzeSf "syntax" [s] pos = throwExc (Type pos "syntax" s)
> analyzeSf "syntax" _ pos = throwExc (Argc pos "syntax")
> analyzeSf _ _ _ = return Nothing

`analyzeFormals` extracts the formal parameter names and possibly the name of a
dotted parameter from the parameter list or symbol.

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

The `ops` map is used to embed primop implementations into the AST.

> ops :: Map.Map String Primop
> ops = Map.fromList [("apply", Applier Primops.apply),
>                     ("call/cc", Applier Primops.callCC),
>                     ("call/vs", Applier Primops.callVs),
>                     ("eval", Evaler eval),
>                     ("values", Purish Primops.values),
>                     ("read/s-all", Impure Primops.readStringAll),
>                     ("write", Impure Primops.write),
>                     ("nl", Impure Primops.nl),
>                     ("open/if", Impure $ Primops.openFP ReadMode),
>                     ("open/of", Impure $ Primops.openFP WriteMode),
>                     ("cline", Impure Primops.cline),
>                     ("eq?", Purish Primops.eq),
>                     ("eqv?", Purish Primops.eqv),
>                     ("equal?", Purish Primops.equal),
>                     ("add", Purish Primops.add),
>                     ("mul", Purish Primops.mul),
>                     ("sub", Purish Primops.sub),
>                     ("lt", Purish Primops.lt),
>                     ("cons", Purish Primops.cons),
>                     ("pair?", Purish Primops.isPair),
>                     ("car", Purish Primops.car),
>                     ("cdr", Purish Primops.cdr),
>                     ("null?", Purish Primops.isNull),
>                     ("mk-stx", Purish Primops.makeSyntax),
>                     ("stx-e", Purish Primops.syntaxExpr),
>                     ("parse/stx-all", Purish Primops.parseSyntaxAll)]

Since the implementation of the `eval` primop uses `analyze` it is found here to
avoid cyclical module dependencies.

> eval :: EvalerImpl
> eval [stx] = do env <- ask
>                 ast <- analyze stx
>                 return (ast, env)
> eval _ = flip Argc "%eval" <$> get >>= throwExc
