> module Hiss.Interpret where
> import Control.Monad.Except (ExceptT, runExceptT, throwError)
> import Control.Monad.State (StateT, evalStateT, get, put)
> import Control.Monad.Reader (ReaderT, runReaderT, ask, asks, local)
> import Control.Monad.Identity (Identity, runIdentity)
> import qualified Data.Map.Strict as Map
> import Data.Array.IArray
> import Hiss.Data

> type EvalState t = StateT Store (ReaderT Env (ExceptT SError Identity)) t

> interpret :: Env -> AST -> Either SError SValue
> interpret e c = runIdentity $ runExceptT
>                 $ runReaderT (evalStateT (eval Halt c) initStore) e

> eval :: Cont -> AST -> EvalState SValue
> eval k (Lambda fs rf body)  = asks (Closure fs rf body) >>= continue k
> eval k (Call f args)        = eval (Fn k args) f
> eval k (If cond conseq alt) = eval (Cond k conseq alt) cond
> eval k (Begin (stmt:stmts)) = eval (Began k stmts) stmt
> eval k (Begin [])           = continue k Unspecified
> eval k (Var name)           = do e <- ask
>                                  s <- get
>                                  case Map.lookup name e of
>                                    Just a -> continue k (deref s a)
>                                    Nothing -> throwError $ Nonbound name
> eval k (Const v)            = continue k v

> continue :: Cont -> SValue -> EvalState SValue
> continue (Fn k (arg:args)) f         = eval (Arg k f [] args) arg
> continue (Fn k []) f                 = apply k f []
> continue (Arg k f as (fm:fms)) a     = eval (Arg k f (a:as) fms) fm
> continue (Arg k f as []) a           = apply k f (reverse (a:as))
> continue (Cond k _ alt) (Bool False) = eval k alt
> continue (Cond k conseq _) _         = eval k conseq
> continue (Began k (stmt:stmts)) _    = eval (Began k stmts) stmt
> continue (Began k []) v              = continue k v
> continue Halt v                      = return v

> apply :: Cont -> SValue -> [SValue] -> EvalState SValue
> apply k (Closure formals restFormal body env) args =
>     do e <- bindArgs env formals restFormal args
>        local (const e) (eval k body)

> bindArgs :: Env -> [String] -> Maybe String -> [SValue] -> EvalState Env
> bindArgs e (f:fs) rf (arg:args) =
>     do a <- alloc arg
>        bindArgs (Map.insert f a e) fs rf args
> bindArgs e [] (Just rf) args = do a <- alloc $ injectList args
>                                   return (Map.insert rf a e)
> bindArgs e [] Nothing [] = return e
> bindArgs _ _ _ _ = throwError Argc

> alloc :: SValue -> EvalState Address -- todo: GC
> alloc v = do (Store s a) <- get
>              put $ Store (s // [(a, v)]) (a + 1)
>              return a

> injectList :: [SValue] -> SValue
> injectList (v:vs) = Pair v (injectList vs)
> injectList [] = Nil
