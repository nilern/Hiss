> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Builtins where
> import Control.Monad (foldM)
> import Control.Monad.State (get)
> import Control.Monad.Except (throwError, liftIO)
> import System.IO (hPrint, stdout)
> import qualified Data.HashTable.IO as H
> import Hiss.Data

> defglobal :: BuiltinImpl
> defglobal [Symbol name, v] = do (e, _) <- get
>                                 liftIO $ H.insert e name v
>                                 return [Unspecified]
> defglobal _ = throwError Argc

> write :: BuiltinImpl
> write [v] = write [v, Port stdout]
> write [v, Port port] = liftIO $ hPrint port v >> return [Unspecified]
> write [_, _] = throwError Type
> write _ = throwError Argc

> add :: PureBuiltinImpl
> add vs = flip (:) [] <$> foldM step (Fixnum 0) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a + b
>           step _ _ = throwError Type

> mul :: PureBuiltinImpl
> mul vs = flip (:) [] <$> foldM step (Fixnum 1) vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a * b
>           step _ _ = throwError Type

> sub :: PureBuiltinImpl
> sub [] = throwError Argc
> sub [Fixnum n] = return [Fixnum $ - n]
> sub [_] = throwError Type
> sub (v:vs) = flip (:) [] <$> foldM step v vs
>     where step (Fixnum a) (Fixnum b) = return $ Fixnum $ a - b
>           step _ _ = throwError Type

> lt :: PureBuiltinImpl
> lt [] = throwError Argc
> lt [Fixnum _] = return [Bool True]
> lt [_] = throwError Type
> lt (v:vs) = flip (:) [] . Bool . snd <$> foldM step (v, True) vs
>     where step (Fixnum a, r) bb@(Fixnum b) = return (bb, r && (a < b))
>           step _ _ = throwError Type
