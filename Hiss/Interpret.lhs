> module Hiss.Interpret where
> import Prelude hiding (lookup)
> import Control.Monad.Except (runExceptT, throwError, lift)
> import Control.Monad.State (evalStateT, get, put)
> import Control.Applicative ((<|>))
> import Hiss.Data

> interpret :: Toplevel -> Store -> AST -> IO (Either SError [SValue])
> interpret e s c = runExceptT $ evalStateT (eval emptyEnv Halt c) (e, s)

> eval :: Env -> Cont -> AST -> EvalState [SValue]
> eval e k (Lambda fs rf body)  = continue k [(Closure fs rf body e)]
> eval e k (Call f args)        = eval e (Fn k e args) f
> eval e k (If cond conseq alt) = eval e (Cond k e conseq alt) cond
> eval e k (Begin (stmt:stmts)) = eval e (Began k e stmts) stmt
> eval _ k (Begin [])           = continue k [Unspecified]
> eval e k (Set name c)         = eval e (SetName k e name) c
> eval e k (Var name)           =
>     do (eg, s) <- get
>        v <- (flip deref s <$> liftThrows (lookup name e))
>             <|> lift (lookupGlobal name eg)
>        continue k [v]
> eval _ k (Const v)            = continue k [v]

> continue :: Cont -> [SValue] -> EvalState [SValue]
> continue (Fn k e (arg:args)) (f:_)       = eval e (Arg k e f [] args) arg
> continue (Fn k _ []) (f:_)               = apply k f []
> continue (Arg k e f as (fm:fms)) (a:_)   = eval e (Arg k e f (a:as) fms) fm
> continue (Arg k _ f as []) (a:_)         = apply k f (reverse (a:as))
> continue (AppVs k f) vs                  = apply k f vs
> continue (Cond k e _ alt) (Bool False:_) = eval e k alt
> continue (Cond k e conseq _) _           = eval e k conseq
> continue (SetName k e name) (v:_)        =
>     do (eg, s) <- get
>        (do a <- liftThrows (lookup name e)
>            put (eg, set a v s))
>          <|> lift (setGlobal name v eg)
>        continue k [Unspecified]
> continue (Began k e (stmt:stmts)) _      = eval e (Began k e stmts) stmt
> continue (Began k _ []) vs               = continue k vs
> continue Halt vs                         = return vs

> apply :: Cont -> SValue -> [SValue] -> EvalState [SValue]
> apply k (Closure formals restFormal body env) args =
>     do e <- bindArgs formals restFormal args env
>        (eval e k body)
> apply k (PureBuiltin f) args = liftThrows (f args) >>= continue k
> apply k (Builtin f) args     = f args >>= continue k
> apply k Apply (f:arglists)   = apply k f $ concatMap ejectList arglists
> apply _ Apply []             = throwError Argc
> apply k CallCC [f]           = apply k f [Continuation k]
> apply _ CallCC _             = throwError Argc
> apply k CallVs [prod, cons]  = apply (AppVs k cons) prod []
> apply _ CallVs _             = throwError Argc
> apply _ (Continuation k) vs  = continue k vs
> apply _ v _                  = throwError $ NonLambda v

> bindArgs :: [String] -> Maybe String -> [SValue] -> Env -> EvalState Env
> bindArgs (f:fs) rf (arg:args) e =
>     do (eg, s) <- get
>        let (a, s') = alloc s arg
>        put (eg, s')
>        bindArgs fs rf args (insert f a e)
> bindArgs [] (Just rf) args e = do (eg, s) <- get
>                                   let (a, s') = alloc s $ injectList args
>                                   put (eg, s')
>                                   return (insert rf a e)
> bindArgs [] Nothing [] e     = return e
> bindArgs _ _ _ _             = throwError Argc
