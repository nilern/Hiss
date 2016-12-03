> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Interpret where
> import Prelude hiding (lookup)
> import Control.Eff hiding (Impure)
> import Control.Eff.State.Lazy
> import Control.Eff.Exception
> import Control.Eff.Lift
> import Control.Eff.Reader.Lazy
> import Text.Parsec.Pos (SourcePos)
> import Hiss.Data

> interpret :: SourcePos -> Env -> AST -> IO (Either SError [SValue])
> interpret pos e c =
>     runLift $ runExc (evalState pos (runReader (eval e (Halt pos) c) e))

> eval :: (Member (State SourcePos) r, Member (Reader Env) r,
>          Member (Exc SError) r, SetMember Lift (Lift IO) r)
>      => Env -> Cont -> AST -> Eff r [SValue]
> eval e k c =
>     do put (positionOf c)
>        case c of
>          Lambda _ fs rf body ->
>              continue k [(Closure fs rf body e)]
>          Call pos f args ->
>              eval e (Fn pos k e args) f
>          Primop pos op (arg:args) ->
>              eval e (PrimArg pos k e op [] args) arg
>          Primop _ op [] ->
>              applyPrimop k op []
>          If pos cond conseq alt ->
>              eval e (Cond pos k e conseq alt) cond
>          Begin pos (stmt:stmts) ->
>              eval e (Began pos k e stmts) stmt
>          Begin _ [] ->
>              continue k [Unspecified]
>          Define pos name v ->
>              eval e (DefineName pos k e name) v
>          Set pos name v ->
>              eval e (SetName pos k e name) v
>          Var _ name ->
>              do v <- lookup e name
>                 continue k [v]
>          Const _ v ->
>              continue k [v]

> continue :: (Member (State SourcePos) r, Member (Reader Env) r,
>              Member (Exc SError) r, SetMember Lift (Lift IO) r)
>          => Cont -> [SValue] -> Eff r [SValue]
> continue cont vals =
>     do put (positionOf cont)
>        case (cont, vals) of
>          (Fn pos k e (arg:args), f:_) ->
>              eval e (Arg pos k e f [] args) arg
>          (Fn _ k _ [], f:_) ->
>              apply k f []
>          (Arg pos k e f as (fm:fms), a:_) ->
>              eval e (Arg pos k e f (a:as) fms) fm
>          (Arg _ k _ f as [], a:_) ->
>              apply k f (reverse (a:as))
>          (PrimArg pos k e op as (fm:fms), a:_) ->
>              eval e (PrimArg pos k e op (a:as) fms) fm
>          (PrimArg _ k _ op as [], a:_) ->
>              applyPrimop k op (reverse (a:as))
>          (AppVs _ k f, vs) ->
>              apply k f vs
>          (Cond _ k e _ alt, Bool False:_) ->
>              eval e k alt
>          (Cond _ k e conseq _, _)  ->
>              eval e k conseq
>          (DefineName _ k e name, v:_) ->
>              define e name v >> continue k [Unspecified]
>          (SetName _ k e name, v:_) ->
>              set e name v >> continue k [Unspecified]
>          (Began pos k e (stmt:stmts), _) ->
>              eval e (Began pos k e stmts) stmt
>          (Began _ k _ [], vs) ->
>              continue k vs
>          (Halt _, vs) ->
>              return vs

> apply :: (Member (State SourcePos) r, Member (Reader Env) r,
>           Member (Exc SError) r, SetMember Lift (Lift IO) r)
>       => Cont -> SValue -> [SValue] -> Eff r [SValue]
> apply k (Closure formals restFormal body env) args =
>     do fas <- zipArgs formals restFormal args
>        env' <- lift $ pushFrame env fas
>        eval env' k body
> apply _ (Continuation k) vs = continue k vs
> apply k Values vs           = continue k vs
> apply _ v _                 = flip NonLambda v <$> get >>= throwExc

> applyPrimop :: (Member (State SourcePos) r, Member (Reader Env) r,
>                 Member (Exc SError) r, SetMember Lift (Lift IO) r)
>             => Cont -> Primop -> [SValue] -> Eff r [SValue]
> applyPrimop k (Purish op) args  = op args >>= continue k
> applyPrimop k (Impure op) args  = op args >>= continue k
> applyPrimop k (Applier op) args = do (k', f, args') <- op k args
>                                      apply k' f args'
> applyPrimop k (Evaler op) args  = do (c, e) <- op args
>                                      eval e k c

> zipArgs :: (Member (State SourcePos) r, Member (Exc SError) r)
>         => [String] -> Maybe String -> [SValue] -> Eff r [(String, SValue)]
> zipArgs (f:fs) rf (arg:args) = (:) (f, arg) <$> zipArgs fs rf args
> zipArgs [] (Just rf) args = return [(rf, injectList args)]
> zipArgs [] Nothing [] = return []
> zipArgs _ _ _ = flip Argc "user-lambda" <$> get >>= throwExc
