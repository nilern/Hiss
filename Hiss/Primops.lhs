> {-# LANGUAGE FlexibleContexts, BangPatterns, GADTs #-}

> module Hiss.Primops where
> import System.IO (hPrint, stdout)
> import System.Mem.StableName
> import Control.Monad (foldM)
> import Control.Eff.State.Lazy
> import Control.Eff.Exception
> import Control.Eff.Lift
> import Hiss.Data

> apply :: ApplierImpl
> apply k (f:args) = return (k, f, concatMap ejectList args)
> apply _ _ = flip Argc "%apply" <$> get >>= throwExc

> callCC :: ApplierImpl
> callCC k [f] = return (k, f, [Continuation k])
> callCC _ _ = flip Argc "%call/cc" <$> get >>= throwExc

> callVs :: ApplierImpl
> callVs k [prod, use] = return (AppVs (positionOf k) k use, prod, [])
> callVs _ _ = flip Argc "%call/vs" <$> get >>= throwExc

> values :: PrimopImpl
> values [] = return [Values]
> values _ = flip Argc "%values" <$> get >>= throwExc

> write :: PrimopImpl
> write [v] = write [v, Port stdout]
> write [v, Port port] = lift $ hPrint port v >> return [Unspecified]
> write [_, _] = Type <$> get >>= throwExc
> write _ = flip Argc "%write" <$> get >>= throwExc

> eq :: PrimopImpl
> eq [!a, !b] =
>     do aName <- lift $ makeStableName a
>        bName <- lift $ makeStableName b
>        if aName == bName
>        then return [Bool True]
>        else case (a, b) of
>             (Bool ab, Bool bb) -> return [Bool (ab == bb)]
>             (Symbol acs, Symbol bcs) -> return [Bool (acs == bcs)]
>             (Nil, Nil) -> return [Bool True]
>             (Unbound, Unbound) -> return [Bool True]
>             (Unspecified, Unspecified) -> return [Bool True]
>             _ -> return [Bool False]
> eq _ = flip Argc "%eq?" <$> get >>= throwExc

> eqv :: PrimopImpl
> eqv [!a, !b] =
>     do aName <- lift $ makeStableName a
>        bName <- lift $ makeStableName b
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
> eqv _ = flip Argc "%eqv?" <$> get >>= throwExc

> equal :: PrimopImpl
> equal [!a, !b] =
>     do aName <- lift $ makeStableName a
>        bName <- lift $ makeStableName b
>        return [Bool $ aName == bName || a == b]
> equal _ = flip Argc "%equal?" <$> get >>= throwExc

> add :: PrimopImpl
> add vs = flip (:) [] <$> foldM step (Fixnum 0) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a + b
>           step _ _ = Type <$> get >>= throwExc

> mul :: PrimopImpl
> mul vs = flip (:) [] <$> foldM step (Fixnum 1) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a * b
>           step _ _ = Type <$> get >>= throwExc

> sub :: PrimopImpl
> sub [] = flip Argc "%-" <$> get >>= throwExc
> sub [Fixnum n] = return [Fixnum $ - n]
> sub [_] = Type <$> get >>= throwExc
> sub (v:vs) = flip (:) [] <$> foldM step v vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a - b
>           step _ _ = Type <$> get >>= throwExc

> lt :: PrimopImpl
> lt [] = flip Argc "%<" <$> get >>= throwExc
> lt [Fixnum _] = return [Bool True]
> lt [_] = Type <$> get >>= throwExc
> lt (v:vs) = flip (:) [] . Bool . snd <$> foldM step (v, True) vs
>     where step (Fixnum a, r) bb@(Fixnum b) = return (bb, r && (a < b))
>           step _ _ = Type <$> get >>= throwExc

> isPair :: PrimopImpl
> isPair [Pair _ _] = return [Bool True]
> isPair [_] = return [Bool False]
> isPair _ = flip Argc "%pair?" <$> get >>= throwExc

> cons :: PrimopImpl
> cons [hd, tl] = return [Pair hd tl]
> cons _ = flip Argc "%cons" <$> get >>= throwExc

> car :: PrimopImpl
> car [Pair hd _] = return [hd]
> car [_] = Type <$> get >>= throwExc
> car _ = flip Argc "%car" <$> get >>= throwExc

> cdr :: PrimopImpl
> cdr [Pair _ tl] = return [tl]
> cdr [_] = Type <$> get >>= throwExc
> cdr _ = flip Argc "%cdr" <$> get >>= throwExc

> isNull :: PrimopImpl
> isNull [Nil] = return [Bool True]
> isNull [_] = return [Bool False]
> isNull _ = flip Argc "%null?" <$> get >>= throwExc

> makeSyntax :: PrimopImpl
> makeSyntax [Syntax _ ctx pos, sexp] = return [Syntax sexp ctx pos]
> makeSyntax [_, _] = Type <$> get >>= throwExc
> makeSyntax _ = flip Argc "%mk-stx" <$> get >>= throwExc

> syntaxExpr :: PrimopImpl
> syntaxExpr [Syntax sexp _ _] = return [sexp]
> syntaxExpr [_] = Type <$> get >>= throwExc
> syntaxExpr _ = flip Argc "%stx-e" <$> get >>= throwExc
