> import Text.ParserCombinators.Parsec (parse)
> import System.Environment (getArgs)
> import Data.List (foldl')
> import Hiss.Data
>        (SValue(Builtin, CallCC), Env, Store, emptyEnv, emptyStore, def)
> import Hiss.Read (datum)
> import Hiss.Analyze (analyze)
> import Hiss.Interpret (interpret)
> import qualified Hiss.Builtins as Builtins

> main :: IO ()
> main = do
>          args <- getArgs
>          expr <- case args of
>                    [filename] -> readFile filename
>                    ["-e", expr] -> return expr
>          case parse datum "hiss" expr of
>            Left err -> putStrLn $ show err
>            Right val -> let (e, s) = initEnvStore in
>                           putStrLn $ show $ interpret e s $ analyze val
>     where initEnvStore = foldl' step (emptyEnv, emptyStore)
>                                 [("+", Builtin Builtins.add),
>                                  ("-", Builtin Builtins.sub),
>                                  ("*", Builtin Builtins.mul),
>                                  ("<", Builtin Builtins.lt),
>                                  ("call/cc", CallCC)]
>           step (e, s) (n, v) = def e s n v
