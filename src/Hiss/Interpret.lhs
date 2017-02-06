
= Evaluator

The evaluator evaluates a very basic version of Scheme. The full version will be
provided by a macro expander frontend written in the restricted language.

> {-# LANGUAGE FlexibleContexts #-}

> module Hiss.Interpret (interpret) where
> import Prelude hiding (lookup)
> import Control.Eff hiding (Impure)
> import Control.Eff.State.Lazy
> import Control.Eff.Exception
> import Control.Eff.Lift
> import Control.Eff.Reader.Lazy
> import Text.Parsec.Pos (SourcePos)
> import Hiss.Data

The evaluator is basically a CEK machine with various effects provided by the
Eff monad. Parts of the machine have also been split from the eval function into
the `continue`, `apply` and `applyPrimop` functions.

The `eval` function evaluates the **c**ontrol expression in an **e**nvironment
with a **k**ontinuation. It does this by simply pattern matching on the control
AST.

Usually it finds a subexpression that needs to be evaluated first. In this case
it uses tail recursion to evaluate the subexpression, using the continuation
argument instead of the Haskell stack to remember what to do next. This ensures
that Scheme tail calls get optimized and also simplifies the implementation of
continuation operations since the continuation is reified at all times.

When a subexpression has been reduced into a value, that value is provided to
`continue` along with the continuation.

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

The `continue` function takes a list of values and a continuation and pattern
matches on the continuation to find out what the values will be used for. The
value list is used to support multiple value returns (`values` and
`call-with-values`).

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

The `apply` function applies a callable value to an argument list. In addition
to closures callables include reified continuations and the `values` 'magic
function' which replace the current continuation or provide it with a
nonsingular number of values, respectfully.

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

`applyPrimop` works just like `apply` except that it applies the primitive
operations defined in `Hiss.Primops`. These are effectively interpreter
intrinsics -- second-class values that can only be called but never passed
around.

> applyPrimop :: (Member (State SourcePos) r, Member (Reader Env) r,
>                 Member (Exc SError) r, SetMember Lift (Lift IO) r)
>             => Cont -> Primop -> [SValue] -> Eff r [SValue]
> applyPrimop k (Purish op) args  = op args >>= continue k
> applyPrimop k (Impure op) args  = op args >>= continue k
> applyPrimop k (Applier op) args = do (k', f, args') <- op k args
>                                      apply k' f args'
> applyPrimop k (Evaler op) args  = do (c, e) <- op args
>                                      eval e k c

`apply` relies on `zipArgs` to bind the actual arguments to formal parameters
when a closure is applied. This is very straightforward except when the function
has a dotted argument list. In that case part of the argument list needs to be
converted into a Scheme list via `injectList`.

> zipArgs :: (Member (State SourcePos) r, Member (Exc SError) r)
>         => [String] -> Maybe String -> [SValue] -> Eff r [(String, SValue)]
> zipArgs (f:fs) rf (arg:args) = (:) (f, arg) <$> zipArgs fs rf args
> zipArgs [] (Just rf) args = return [(rf, injectList args)]
> zipArgs [] Nothing [] = return []
> zipArgs _ _ _ = flip Argc "user-lambda" <$> get >>= throwExc

`eval` does not actually perform any effects but just builds up a representation
of the effectful computation (a 'Freer Monad'). In order to actually perform the
effects or get at the final return value(s) we need to wrap `eval` in calls to
the actual effect-running functions. We call this more convenient version of
`eval` `interpret` and expose it as the API of this module.

> interpret :: SourcePos -> Env -> AST -> IO (Either SError [SValue])
> interpret pos e c =
>     runLift $ runExc (evalState pos (runReader (eval e (Halt pos) c) e))
