> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE BangPatterns #-}

> module Hiss.Primops where
> import System.Mem.StableName
> import Control.Monad (foldM)
> import Control.Monad.State (get)
> import Control.Monad.Except (throwError, liftIO)
> import System.IO (hPrint, stdout)
> import qualified Data.Map.Strict as Map
> import qualified Data.HashTable.IO as H
> import Hiss.Data

> apply :: ApplierImpl
> apply k (f:args) = return (k, f, concatMap ejectList args)
> apply _ _ = flip Argc "%apply" <$> getPos >>= throwError

> callCC :: ApplierImpl
> callCC k [f] = return (k, f, [Continuation k])
> callCC _ _ = flip Argc "%call/cc" <$> getPos >>= throwError

> callVs :: ApplierImpl
> callVs k [prod, use] = return (AppVs (positionOf k) k use, prod, [])
> callVs _ _ = flip Argc "%call/vs" <$> getPos >>= throwError

> values :: PrimopImpl
> values [] = return [Values]
> values _ = flip Argc "%values" <$> getPos >>= throwError

> defglobal :: PrimopImpl
> defglobal [Symbol name, v] = do (e, _, _) <- get
>                                 liftIO $ H.insert e name v
>                                 return [Unspecified]
> defglobal [_, _] = Type <$> getPos >>= throwError
> defglobal _ = flip Argc "%defglobal" <$> getPos >>= throwError

> write :: PrimopImpl
> write [v] = write [v, Port stdout]
> write [v, Port port] = liftIO $ hPrint port v >> return [Unspecified]
> write [_, _] = Type <$> getPos >>= throwError
> write _ = flip Argc "%write" <$> getPos >>= throwError

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
> eq _ = flip Argc "%eq?" <$> getPos >>= throwError

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
> eqv _ = flip Argc "%eqv?" <$> getPos >>= throwError

> equal :: PrimopImpl
> equal [!a, !b] =
>     do aName <- liftIO $ makeStableName a
>        bName <- liftIO $ makeStableName b
>        return [Bool $ aName == bName || a == b]
> equal _ = flip Argc "%equal?" <$> getPos >>= throwError

> add :: PrimopImpl
> add vs = flip (:) [] <$> foldM step (Fixnum 0) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a + b
>           step _ _ = Type <$> getPos >>= throwError

> mul :: PrimopImpl
> mul vs = flip (:) [] <$> foldM step (Fixnum 1) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a * b
>           step _ _ = Type <$> getPos >>= throwError

> sub :: PrimopImpl
> sub [] = flip Argc "%-" <$> getPos >>= throwError
> sub [Fixnum n] = return [Fixnum $ - n]
> sub [_] = Type <$> getPos >>= throwError
> sub (v:vs) = flip (:) [] <$> foldM step v vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a - b
>           step _ _ = Type <$> getPos >>= throwError

> lt :: PrimopImpl
> lt [] = flip Argc "%<" <$> getPos >>= throwError
> lt [Fixnum _] = return [Bool True]
> lt [_] = Type <$> getPos >>= throwError
> lt (v:vs) = flip (:) [] . Bool . snd <$> foldM step (v, True) vs
>     where step (Fixnum a, r) bb@(Fixnum b) = return (bb, r && (a < b))
>           step _ _ = Type <$> getPos >>= throwError

> isPair :: PrimopImpl
> isPair [Pair _ _] = return [Bool True]
> isPair [_] = return [Bool False]
> isPair _ = flip Argc "%pair?" <$> getPos >>= throwError

> cons :: PrimopImpl
> cons [hd, tl] = return [Pair hd tl]
> cons _ = flip Argc "%cons" <$> getPos >>= throwError

> car :: PrimopImpl
> car [Pair hd _] = return [hd]
> car [_] = Type <$> getPos >>= throwError
> car _ = flip Argc "%car" <$> getPos >>= throwError

> cdr :: PrimopImpl
> cdr [Pair _ tl] = return [tl]
> cdr [_] = Type <$> getPos >>= throwError
> cdr _ = flip Argc "%cdr" <$> getPos >>= throwError

> isNull :: PrimopImpl
> isNull [Nil] = return [Bool True]
> isNull [_] = return [Bool False]
> isNull _ = flip Argc "%null?" <$> getPos >>= throwError

> makeSyntax :: PrimopImpl
> makeSyntax [Syntax _ ctx pos, sexp] = return [Syntax sexp ctx pos]
> makeSyntax [_, _] = Type <$> getPos >>= throwError
> makeSyntax _ = flip Argc "%mk-stx" <$> getPos >>= throwError

> syntaxExpr :: PrimopImpl
> syntaxExpr [Syntax sexp _ _] = return [sexp]
> syntaxExpr [_] = Type <$> getPos >>= throwError
> syntaxExpr _ = flip Argc "%stx-e" <$> getPos >>= throwError
