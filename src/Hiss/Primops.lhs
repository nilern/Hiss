
= Primitive Operations

This file contains implementations of primitive operations. These operations are
not "interesting" enough to be part of the evaluator but nevertheless need to be
implemented at this level because they perform side effects or would be too slow
if they were implemented in Scheme using only the operations the evaluator does
provide (via Church-encoding, closure objects and similar trickery).

> {-# LANGUAGE FlexibleContexts, BangPatterns, GADTs, RankNTypes, MagicHash #-}

> module Hiss.Primops where
> import Text.Parsec (parse)
> import System.IO (hPutStr, hPutStrLn, hGetContents, stdout, openFile,
>                   IOMode(ReadMode, WriteMode))
> import System.Environment (getProgName, getArgs)
> import GHC.Prim (reallyUnsafePtrEquality#)
> import Control.Monad (foldM)
> import Control.Eff.State.Lazy
> import Control.Eff.Exception
> import Control.Eff.Lift
> import Hiss.Data
> import Hiss.Read (datums)

> apply :: ApplierImpl
> apply k (f:args) = return (k, f, concatMap ejectList args)
> apply _ _ = flip Argc "%apply" <$> get >>= throwExc

> callCC :: ApplierImpl
> callCC k [f] = return (k, f, [Continuation k])
> callCC _ _ = flip Argc "%call/cc" <$> get >>= throwExc

> callVs :: ApplierImpl
> callVs k [prod, use] = return (AppVs (positionOf k) k use, prod, [])
> callVs _ _ = flip Argc "%call/vs" <$> get >>= throwExc

> values :: ExcImpl
> values [] = return [Values]
> values _ = flip Argc "%values" <$> get >>= throwExc

> readStringAll :: PrimopImpl
> readStringAll [Port port] = flip (:) [] . String <$> lift (hGetContents port)
> readStringAll [p] = do pos <- get
>                        throwExc $ Type pos "port" p
> readStringAll _ = flip Argc "%read/s-all" <$> get >>= throwExc

> write :: PrimopImpl
> write [v] = write [v, Port stdout]
> write [v, Port port] = lift $ hPutStr port (show v) >> return [Unspecified]
> write [_, p] = do pos <- get
>                   throwExc $ Type pos "port" p
> write _ = flip Argc "%write" <$> get >>= throwExc

> nl :: PrimopImpl
> nl [] = nl [Port stdout]
> nl [Port port] = lift $ hPutStrLn port "" >> return [Unspecified]
> nl [p] = do pos <- get
>             throwExc $ Type pos "port" p
> nl _ = flip Argc "%nl" <$> get >>= throwExc

> openFP :: IOMode -> PrimopImpl
> openFP mode [String filename] = do fh <- lift $ openFile filename mode
>                                    return [Port fh]
> openFP _ [p] = do pos <- get
>                   throwExc $ Type pos "port" p
> openFP ReadMode _ = flip Argc "%open/if" <$> get >>= throwExc
> openFP WriteMode _ = flip Argc "%open/of" <$> get >>= throwExc

> cline :: PrimopImpl
> cline [] = do progName <- lift getProgName
>               cliArgs <- lift getArgs
>               return [injectList $ map String $ progName : cliArgs]
> cline _ = flip Argc "%cline" <$> get >>= throwExc

> eq :: ExcImpl
> eq [!a, !b] =
>     case reallyUnsafePtrEquality# a b of
>       1# -> return [Bool True]
>       _ -> case (a, b) of
>              (Bool ab, Bool bb) -> return [Bool (ab == bb)]
>              (Symbol acs, Symbol bcs) -> return [Bool (acs == bcs)]
>              (Nil, Nil) -> return [Bool True]
>              (Unbound, Unbound) -> return [Bool True]
>              (Unspecified, Unspecified) -> return [Bool True]
>              _ -> return [Bool False]
> eq _ = flip Argc "%eq?" <$> get >>= throwExc

> eqv :: ExcImpl
> eqv [!a, !b] =
>     case reallyUnsafePtrEquality# a b of
>       1# -> return [Bool True]
>       _ -> case (a, b) of
>              (Bool ab, Bool bb) -> return [Bool (ab == bb)]
>              (Symbol acs, Symbol bcs) -> return [Bool (acs == bcs)]
>              (Nil, Nil) -> return [Bool True]
>              (Fixnum n, Fixnum m) -> return [Bool (n == m)]
>              (Unbound, Unbound) -> return [Bool True]
>              (Unspecified, Unspecified) -> return [Bool True]
>              _ -> return [Bool False]
> eqv _ = flip Argc "%eqv?" <$> get >>= throwExc

> equal :: ExcImpl
> equal [!a, !b] =
>     case reallyUnsafePtrEquality# a b of
>       1# -> return [Bool True]
>       _ -> return [Bool $ a == b]
> equal _ = flip Argc "%equal?" <$> get >>= throwExc

> add :: ExcImpl
> add vs = flip (:) [] <$> foldM step (Fixnum 0) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a + b
>           step a (Fixnum _) = do pos <- get
>                                  throwExc $ Type pos "fixnum" a
>           step (Fixnum _) b = do pos <- get
>                                  throwExc $ Type pos "fixnum" b
>           step a _ = do pos <- get
>                         throwExc $ Type pos "fixnum" a

> mul :: ExcImpl
> mul vs = flip (:) [] <$> foldM step (Fixnum 1) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a * b
>           step a (Fixnum _) = do pos <- get
>                                  throwExc $ Type pos "fixnum" a
>           step (Fixnum _) b = do pos <- get
>                                  throwExc $ Type pos "fixnum" b
>           step a _ = do pos <- get
>                         throwExc $ Type pos "fixnum" a

> sub :: ExcImpl
> sub [] = flip Argc "%-" <$> get >>= throwExc
> sub [Fixnum n] = return [Fixnum $ - n]
> sub [n] = do pos <- get
>              throwExc $ Type pos "fixnum" n
> sub (v:vs) = flip (:) [] <$> foldM step v vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a - b
>           step a (Fixnum _) = do pos <- get
>                                  throwExc $ Type pos "fixnum" a
>           step (Fixnum _) b = do pos <- get
>                                  throwExc $ Type pos "fixnum" b
>           step a _ = do pos <- get
>                         throwExc $ Type pos "fixnum" a

> lt :: ExcImpl
> lt [] = flip Argc "%<" <$> get >>= throwExc
> lt [Fixnum _] = return [Bool True]
> lt [n] = do pos <- get
>             throwExc $ Type pos "fixnum" n
> lt (v:vs) = flip (:) [] . Bool . snd <$> foldM step (v, True) vs
>     where step (Fixnum a, r) bb@(Fixnum b) = return (bb, r && (a < b))
>           step (a, _) (Fixnum _) = do pos <- get
>                                       throwExc $ Type pos "fixnum" a
>           step (Fixnum _, _) b = do pos <- get
>                                     throwExc $ Type pos "fixnum" b
>           step (a, _) _ = do pos <- get
>                              throwExc $ Type pos "fixnum" a

> isPair :: ExcImpl
> isPair [Pair _ _] = return [Bool True]
> isPair [_] = return [Bool False]
> isPair _ = flip Argc "%pair?" <$> get >>= throwExc

> cons :: ExcImpl
> cons [hd, tl] = return [Pair hd tl]
> cons _ = flip Argc "%cons" <$> get >>= throwExc

> car :: ExcImpl
> car [Pair hd _] = return [hd]
> car [p] = do pos <- get
>              throwExc $ Type pos "pair" p
> car _ = flip Argc "%car" <$> get >>= throwExc

> cdr :: ExcImpl
> cdr [Pair _ tl] = return [tl]
> cdr [p] = do pos <- get
>              throwExc $ Type pos "pair" p
> cdr _ = flip Argc "%cdr" <$> get >>= throwExc

> isNull :: ExcImpl
> isNull [Nil] = return [Bool True]
> isNull [_] = return [Bool False]
> isNull _ = flip Argc "%null?" <$> get >>= throwExc

> makeSyntax :: ExcImpl
> makeSyntax [Syntax _ ctx pos, sexp] = return [Syntax sexp ctx pos]
> makeSyntax [s, _] = do pos <- get
>                        throwExc $ Type pos "syntax" s
> makeSyntax _ = flip Argc "%mk-stx" <$> get >>= throwExc

> syntaxExpr :: ExcImpl
> syntaxExpr [Syntax sexp _ _] = return [sexp]
> syntaxExpr [s] = do pos <- get
>                     throwExc $ Type pos "syntax" s
> syntaxExpr _ = flip Argc "%stx-e" <$> get >>= throwExc

> parseSyntaxAll :: ExcImpl
> parseSyntaxAll [String desc, String s] =
>     case parse datums desc s of
>       Right vals -> return [vals]
>       Left err -> flip ParseError err <$> get >>= throwExc
> parseSyntaxAll [String _, s] = do pos <- get
>                                   throwExc $ Type pos "string" s
> parseSyntaxAll [s, String _] = do pos <- get
>                                   throwExc $ Type pos "string" s
> parseSyntaxAll _ = flip Argc "%parse/stx-all" <$> get >>= throwExc
