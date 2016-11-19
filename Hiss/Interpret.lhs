> module Hiss.Interpret where
> import Control.Monad.Except (ExceptT, runExceptT, throwError)
> import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
> import Control.Monad.Identity (Identity, runIdentity)
> import Data.Map.Strict as Map
> import Hiss.Data

> type EvalState = ReaderT Env (ExceptT SError Identity) SValue

> interpret :: Env -> AST -> Either SError SValue
> interpret e c = runIdentity $ runExceptT $ runReaderT (eval Halt c) e

> eval :: Cont -> AST -> EvalState
> eval k (Lambda fs rf body) = asks (Closure fs rf body) >>= continue k
> eval k (Call f args) = eval (Fn k args) f
> eval k (If cond conseq alt) = eval (Cond k conseq alt) cond
> eval k (Begin (stmt:stmts)) = eval (Began k stmts) stmt
> eval k (Begin []) = continue k Unspecified
> eval k (Var name) = asks (Map.lookup name) >>= examine
>     where examine (Just v) = continue k v
>           examine Nothing = throwError $ Unbound name
> eval k (Const v) = continue k v

> continue :: Cont -> SValue -> EvalState
> continue (Fn k (arg:args)) f = eval (Arg k f [] args) arg
> continue (Fn k []) f = apply f [] k
> continue (Arg k f as (fm:fms)) a = eval (Arg k f (a:as) fms) fm
> continue (Arg k f as []) a = apply f (reverse (a:as)) k
> continue (Cond k _ alt) (Bool False) = eval k alt
> continue (Cond k conseq _) _ = eval k conseq
> continue (Began k (stmt:stmts)) _ = eval (Began k stmts) stmt
> continue (Began k []) v = continue k v
> continue Halt v = return v

> apply :: SValue -> [SValue] -> Cont -> EvalState
> apply (Closure formals restFormal body e0) args k =
>     case bindArgs e0 formals restFormal args of
>        Just e -> local (const e) (eval k body)
>        Nothing -> throwError Argc
>     where bindArgs e (f:fs) rf (a:as) = bindArgs (Map.insert f a e) fs rf as
>           bindArgs e [] _ [] = Just e
>           bindArgs e [] (Just rf) as = Just $ Map.insert rf (injectList as) e
>           bindArgs _ _ _ _ = Nothing
> apply f _ _ = throwError $ NonLambda f

> injectList :: [SValue] -> SValue
> injectList (v:vs) = Pair v (injectList vs)
> injectList [] = Nil
