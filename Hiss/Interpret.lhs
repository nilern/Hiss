> module Hiss.Interpret where
> import Prelude hiding (lookup)
> import Text.Parsec.Pos (SourcePos)
> import Control.Monad.Except (runExceptT, throwError, lift)
> import Control.Monad.State (evalStateT, get, put)
> import Control.Applicative ((<|>))
> import Hiss.Data

> interpret :: SourcePos -> Toplevel -> Store -> AST -> IO (Either SError [SValue])
> interpret pos e s c = runExceptT
>                       $ evalStateT (eval emptyEnv (Halt pos) c) (e, s, pos)

> eval :: Env -> Cont -> AST -> EvalState [SValue]
> eval e k c =
>     do putPos (positionOf c)
>        case c of
>          Lambda _ fs rf body -> continue k [(Closure fs rf body e)]
>          Call pos f args -> eval e (Fn pos k e args) f
>          Primop pos op (arg:args) -> eval e (PrimArg pos k e op [] args) arg
>          Primop _ op [] -> applyPrimop k op []
>          If pos cond conseq alt -> eval e (Cond pos k e conseq alt) cond
>          Begin pos (stmt:stmts) -> eval e (Began pos k e stmts) stmt
>          Begin _ [] -> continue k [Unspecified]
>          Set pos name v -> eval e (SetName pos k e name) v
>          Var pos name ->
>              do (eg, s, _) <- get
>                 v <- (flip deref s <$> liftThrows (lookup name e pos))
>                      <|> lift (lookupGlobal name eg pos)
>                 continue k [v]
>          Const _ v -> continue k [v]

> continue :: Cont -> [SValue] -> EvalState [SValue]
> continue (Fn pos k e (arg:args)) (f:_) =
>     putPos pos >> eval e (Arg pos k e f [] args) arg
> continue (Fn pos k _ []) (f:_) =
>     putPos pos >> apply k f []
> continue (Arg pos k e f as (fm:fms)) (a:_) =
>     putPos pos >> eval e (Arg pos k e f (a:as) fms) fm
> continue (Arg pos k _ f as []) (a:_) =
>     putPos pos >> apply k f (reverse (a:as))
> continue (PrimArg pos k e op as (fm:fms)) (a:_) =
>     putPos pos >> eval e (PrimArg pos k e op (a:as) fms) fm
> continue (PrimArg pos k _ op as []) (a:_) =
>     putPos pos >> applyPrimop k op (reverse (a:as))
> continue (AppVs pos k f) vs =
>     putPos pos >> apply k f vs
> continue (Cond pos k e _ alt) (Bool False:_) =
>     putPos pos >> eval e k alt
> continue (Cond pos k e conseq _) _ =
>     putPos pos >> eval e k conseq
> continue (SetName pos k e name) (v:_) =
>     do putPos pos
>        (eg, s, _) <- get
>        (do a <- liftThrows (lookup name e pos)
>            put (eg, set a v s, pos))
>          <|> lift (setGlobal name v eg pos)
>        continue k [Unspecified]
> continue (Began pos k e (stmt:stmts)) _ =
>     putPos pos >> eval e (Began pos k e stmts) stmt
> continue (Began pos k _ []) vs =
>     putPos pos >> continue k vs
> continue (Halt pos) vs =
>     putPos pos >> return vs

> apply :: Cont -> SValue -> [SValue] -> EvalState [SValue]
> apply k (Closure formals restFormal body env) args =
>     do e <- bindArgs formals restFormal args env
>        (eval e k body)
> apply _ (Continuation k) vs = continue k vs
> apply _ v _                 = flip NonLambda v <$> getPos >>= throwError

> applyPrimop :: Cont -> Primop -> [SValue] -> EvalState [SValue]
> applyPrimop k (Impure op) args  = op args >>= continue k
> applyPrimop k (Applier op) args = do (k', f, args') <- op k args
>                                      apply k' f args'

> bindArgs :: [String] -> Maybe String -> [SValue] -> Env -> EvalState Env
> bindArgs (f:fs) rf (arg:args) e =
>     do (eg, s, pos) <- get
>        let (a, s') = alloc s arg
>        put (eg, s', pos)
>        bindArgs fs rf args (insert f a e)
> bindArgs [] (Just rf) args e = do (eg, s, pos) <- get
>                                   let (a, s') = alloc s $ injectList args
>                                   put (eg, s', pos)
>                                   return (insert rf a e)
> bindArgs [] Nothing [] e     = return e
> bindArgs _ _ _ _             = Argc <$> getPos >>= throwError
