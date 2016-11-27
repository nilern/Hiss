> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Primops where
> import Control.Monad (foldM)
> import Control.Monad.State (get)
> import Control.Monad.Except (throwError, liftIO)
> import System.IO (hPrint, stdout)
> import qualified Data.Map.Strict as Map
> import qualified Data.HashTable.IO as H
> import Hiss.Data

> ops :: Map.Map String Primop
> ops = Map.fromList [("apply", Applier apply),
>                     ("call/cc", Applier callCC),
>                     ("call/vs", Applier callVs),
>                     ("values", Pure values),
>                     ("defglobal", Impure defglobal),
>                     ("write", Impure write),
>                     ("eq?", Pure eq),
>                     ("add", Pure add),
>                     ("mul", Pure mul),
>                     ("sub", Pure sub),
>                     ("lt", Pure lt),
>                     ("cons", Pure cons),
>                     ("pair?", Pure isPair),
>                     ("car", Pure car),
>                     ("cdr", Pure cdr)]

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

> eq :: PurePrimopImpl
> eq [Symbol a, Symbol b] = return [Bool (a == b)]

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

> isPair :: PurePrimopImpl
> isPair [Pair _ _] = return [Bool True]
> isPair [_] = return [Bool False]
> isPair _ = throwError Argc

> cons :: PurePrimopImpl
> cons [hd, tl] = return [Pair hd tl]
> cons _ = throwError Argc

> car :: PurePrimopImpl
> car [Pair hd _] = return [hd]
> car [_] = throwError Type
> car _ = throwError Argc

> cdr :: PurePrimopImpl
> cdr [Pair _ tl] = return [tl]
> cdr [_] = throwError Type
> cdr _ = throwError Argc
