> module Hiss.Interpret where
> import Control.Monad.Except (runExceptT, throwError)
> import Control.Monad.State (evalStateT, get, put)
> import qualified Data.Map.Strict as Map
> import Hiss.Data

> interpret :: Env -> Store -> AST -> IO (Either SError SValue)
> interpret e s c = runExceptT $ evalStateT (eval emptyEnv Halt c) (e, s)

> eval :: Env -> Cont -> AST -> EvalState SValue
> eval e k (Lambda fs rf body)  = continue k (Closure fs rf body e)
> eval e k (Call f args)        = eval e (Fn k e args) f
> eval e k (If cond conseq alt) = eval e (Cond k e conseq alt) cond
> eval e k (Begin (stmt:stmts)) = eval e (Began k e stmts) stmt
> eval _ k (Begin [])           = continue k Unspecified
> eval e k (Set name c)         = eval e (SetName k e name) c
> eval e k (Var name)           =
>     do (eg, s) <- get
>        case Map.lookup name e of
>          Just a -> continue k $ deref a s
>          Nothing -> case Map.lookup name eg of
>                       Just a -> continue k $ deref a s
>                       Nothing -> throwError $ Nonbound name
> eval _ k (Const v)            = continue k v

> continue :: Cont -> SValue -> EvalState SValue
> continue (Fn k e (arg:args)) f         = eval e (Arg k e f [] args) arg
> continue (Fn k _ []) f                 = apply k f []
> continue (Arg k e f as (fm:fms)) a     = eval e (Arg k e f (a:as) fms) fm
> continue (Arg k _ f as []) a           = apply k f (reverse (a:as))
> continue (Cond k e _ alt) (Bool False) = eval e k alt
> continue (Cond k e conseq _) _         = eval e k conseq
> continue (SetName k e name) v          =
>     do (eg, s) <- get
>        case (Map.lookup name e) of
>          Just a -> put (eg, set a v s) >> continue k Unspecified
>          Nothing -> case Map.lookup name eg of
>                       Just a -> put (eg, set a v s) >> continue k Unspecified
>                       Nothing -> throwError $ Nonbound name
> continue (Began k e (stmt:stmts)) _    = eval e (Began k e stmts) stmt
> continue (Began k _ []) v              = continue k v
> continue Halt v                        = return v

> apply :: Cont -> SValue -> [SValue] -> EvalState SValue
> apply k (Closure formals restFormal body env) args =
>     do e <- bindArgs formals restFormal args env
>        (eval e k body)
> apply k (Builtin f) args     = f args >>= continue k
> apply k CallCC [f]           = apply k f [(Continuation k)]
> apply _ CallCC _             = throwError Argc
> apply _ (Continuation k) [v] = continue k v
> apply _ (Continuation _) _   = throwError Argc
> apply _ v _                  = throwError $ NonLambda v

> bindArgs :: [String] -> Maybe String -> [SValue] -> Env -> EvalState Env
> bindArgs (f:fs) rf (arg:args) e =
>     do (eg, s) <- get
>        let (a, s') = alloc s arg
>        put (eg, s')
>        bindArgs fs rf args (Map.insert f a e)
> bindArgs [] (Just rf) args e = do (eg, s) <- get
>                                   let (a, s') = alloc s $ injectList args
>                                   put (eg, s')
>                                   return (Map.insert rf a e)
> bindArgs [] Nothing [] e     = return e
> bindArgs _ _ _ _             = throwError Argc
