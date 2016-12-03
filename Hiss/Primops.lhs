> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE BangPatterns #-}

> module Hiss.Primops where
> import System.Mem.StableName
> import Control.Monad (foldM)
> import Control.Monad.State (get)
> import Control.Monad.Except (throwError, liftIO)
> import System.IO (hPrint, stdout)
> import Hiss.Data

> apply :: ApplierImpl
> apply k (f:args) = return (k, f, concatMap ejectList args)
> apply _ _ = flip Argc "%apply" <$> get >>= throwError

> callCC :: ApplierImpl
> callCC k [f] = return (k, f, [Continuation k])
> callCC _ _ = flip Argc "%call/cc" <$> get >>= throwError

> callVs :: ApplierImpl
> callVs k [prod, use] = return (AppVs (positionOf k) k use, prod, [])
> callVs _ _ = flip Argc "%call/vs" <$> get >>= throwError

> values :: PrimopImpl
> values [] = return [Values]
> values _ = flip Argc "%values" <$> get >>= throwError

> write :: PrimopImpl
> write [v] = write [v, Port stdout]
> write [v, Port port] = liftIO $ hPrint port v >> return [Unspecified]
> write [_, _] = Type <$> get >>= throwError
> write _ = flip Argc "%write" <$> get >>= throwError

> eq :: PrimopImpl
> eq [!a, !b] =
>     do aName <- liftIO $ makeStableName a
>        bName <- liftIO $ makeStableName b
>        if aName == bName
>        then return [Bool True]
>        else case (a, b) of
>             (Bool ab, Bool bb) -> return [Bool (ab == bb)]
>             (Symbol acs, Symbol bcs) -> return [Bool (acs == bcs)]
>             (Nil, Nil) -> return [Bool True]
>             (Unbound, Unbound) -> return [Bool True]
>             (Unspecified, Unspecified) -> return [Bool True]
>             _ -> return [Bool False]
> eq _ = flip Argc "%eq?" <$> get >>= throwError

> eqv :: PrimopImpl
> eqv [!a, !b] =
>     do aName <- liftIO $ makeStableName a
>        bName <- liftIO $ makeStableName b
>        if aName == bName
>        then return [Bool True]
>        else case (a, b) of
>             (Bool ab, Bool bb) -> return [Bool (ab == bb)]
>             (Symbol acs, Symbol bcs) -> return [Bool (acs == bcs)]
>             (Nil, Nil) -> return [Bool True]
>             (Fixnum n, Fixnum m) -> return [Bool (n == m)]
>             (Unbound, Unbound) -> return [Bool True]
>             (Unspecified, Unspecified) -> return [Bool True]
>             _ -> return [Bool False]
> eqv _ = flip Argc "%eqv?" <$> get >>= throwError

> equal :: PrimopImpl
> equal [!a, !b] =
>     do aName <- liftIO $ makeStableName a
>        bName <- liftIO $ makeStableName b
>        return [Bool $ aName == bName || a == b]
> equal _ = flip Argc "%equal?" <$> get >>= throwError

> add :: PrimopImpl
> add vs = flip (:) [] <$> foldM step (Fixnum 0) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a + b
>           step _ _ = Type <$> get >>= throwError

> mul :: PrimopImpl
> mul vs = flip (:) [] <$> foldM step (Fixnum 1) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a * b
>           step _ _ = Type <$> get >>= throwError

> sub :: PrimopImpl
> sub [] = flip Argc "%-" <$> get >>= throwError
> sub [Fixnum n] = return [Fixnum $ - n]
> sub [_] = Type <$> get >>= throwError
> sub (v:vs) = flip (:) [] <$> foldM step v vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a - b
>           step _ _ = Type <$> get >>= throwError

> lt :: PrimopImpl
> lt [] = flip Argc "%<" <$> get >>= throwError
> lt [Fixnum _] = return [Bool True]
> lt [_] = Type <$> get >>= throwError
> lt (v:vs) = flip (:) [] . Bool . snd <$> foldM step (v, True) vs
>     where step (Fixnum a, r) bb@(Fixnum b) = return (bb, r && (a < b))
>           step _ _ = Type <$> get >>= throwError

> isPair :: PrimopImpl
> isPair [Pair _ _] = return [Bool True]
> isPair [_] = return [Bool False]
> isPair _ = flip Argc "%pair?" <$> get >>= throwError

> cons :: PrimopImpl
> cons [hd, tl] = return [Pair hd tl]
> cons _ = flip Argc "%cons" <$> get >>= throwError

> car :: PrimopImpl
> car [Pair hd _] = return [hd]
> car [_] = Type <$> get >>= throwError
> car _ = flip Argc "%car" <$> get >>= throwError

> cdr :: PrimopImpl
> cdr [Pair _ tl] = return [tl]
> cdr [_] = Type <$> get >>= throwError
> cdr _ = flip Argc "%cdr" <$> get >>= throwError

> isNull :: PrimopImpl
> isNull [Nil] = return [Bool True]
> isNull [_] = return [Bool False]
> isNull _ = flip Argc "%null?" <$> get >>= throwError

> makeSyntax :: PrimopImpl
> makeSyntax [Syntax _ ctx pos, sexp] = return [Syntax sexp ctx pos]
> makeSyntax [_, _] = Type <$> get >>= throwError
> makeSyntax _ = flip Argc "%mk-stx" <$> get >>= throwError

> syntaxExpr :: PrimopImpl
> syntaxExpr [Syntax sexp _ _] = return [sexp]
> syntaxExpr [_] = Type <$> get >>= throwError
> syntaxExpr _ = flip Argc "%stx-e" <$> get >>= throwError
