> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Builtins where
> import Control.Monad (foldM)
> import Control.Monad.Except (throwError)
> import Hiss.Data

> add :: BuiltinImpl
> add vs = foldM step (Fixnum 0) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a + b
>           step _ _ = throwError Type

> mul :: BuiltinImpl
> mul vs = foldM step (Fixnum 1) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a * b
>           step _ _ = throwError Type

> sub :: BuiltinImpl
> sub [] = throwError Argc
> sub [Fixnum n] = return $ Fixnum $ - n
> sub [_] = throwError Type
> sub (v:vs) = foldM step v vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a - b
>           step _ _ = throwError Type

> lt :: BuiltinImpl
> lt [] = throwError Argc
> lt [Fixnum n] = return $ Bool True
> lt [_] = throwError Type
> lt (v:vs) = (\(_, b) -> Bool b) <$> foldM step (v, True) vs
>     where step (Fixnum a, r) bb@(Fixnum b) = return (bb, r && (a < b))
>           step _ _ = throwError Type
