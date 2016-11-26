> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Builtins where
> import Control.Monad (foldM)
> import Control.Monad.State (get)
> import Control.Monad.Except (throwError, liftIO)
> import System.IO (hPrint, stdout)
> import qualified Data.HashTable.IO as H
> import Hiss.Data

> apply :: ApplierImpl
> apply k (f:args) = return (k, f, concatMap ejectList args)
> apply _ _ = throwError Argc

> callCC :: ApplierImpl
> callCC k [f] = return (k, f, [Continuation k])
> callCC _ _ = throwError Argc

> callVs :: ApplierImpl
> callVs k [prod, cons] = return (AppVs k cons, prod, [])
> callVs _ _ = throwError Argc

> values :: PurePrimopImpl
> values = return

> defglobal :: PrimopImpl
> defglobal [Symbol name, v] = do (e, _) <- get
>                                 liftIO $ H.insert e name v
>                                 return [Unspecified]
> defglobal [_, _] = throwError Type
> defglobal _ = throwError Argc

> write :: PrimopImpl
> write [v] = write [v, Port stdout]
> write [v, Port port] = liftIO $ hPrint port v >> return [Unspecified]
> write [_, _] = throwError Type
> write _ = throwError Argc

> add :: PurePrimopImpl
> add vs = flip (:) [] <$> foldM step (Fixnum 0) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a + b
>           step _ _ = throwError Type

> mul :: PurePrimopImpl
> mul vs = flip (:) [] <$> foldM step (Fixnum 1) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a * b
>           step _ _ = throwError Type

> sub :: PurePrimopImpl
> sub [] = throwError Argc
> sub [Fixnum n] = return [Fixnum $ - n]
> sub [_] = throwError Type
> sub (v:vs) = flip (:) [] <$> foldM step v vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a - b
>           step _ _ = throwError Type

> lt :: PurePrimopImpl
> lt [] = throwError Argc
> lt [Fixnum _] = return [Bool True]
> lt [_] = throwError Type
> lt (v:vs) = flip (:) [] . Bool . snd <$> foldM step (v, True) vs
>     where step (Fixnum a, r) bb@(Fixnum b) = return (bb, r && (a < b))
>           step _ _ = throwError Type
