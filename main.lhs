> import Text.ParserCombinators.Parsec (parse)
> import System.Environment (getArgs)
> import Data.List (foldl')
> import Hiss.Data (SValue(Symbol, Builtin, CallCC, CallVs, Apply),
>                  emptyEnv, emptyStore, def, injectList)
> import Hiss.Read (datums)
> import Hiss.Analyze (analyze)
> import Hiss.Interpret (interpret)
> import qualified Hiss.Builtins as Builtins

> main :: IO ()
> main = do
>          args <- getArgs
>          expr <- case args of
>                    [filename] -> readFile filename
>                    ["-e", expr] -> return expr
>          case parse datums "hiss" expr of
>            Left err -> putStrLn $ show err
>            Right vals -> let (e, s) = initEnvStore in
>                            evalPrint e s $ injectList (Symbol "begin" : vals)
>     where initEnvStore = foldl' step (emptyEnv, emptyStore)
>                                 [("+", Builtin Builtins.add),
>                                  ("-", Builtin Builtins.sub),
>                                  ("*", Builtin Builtins.mul),
>                                  ("<", Builtin Builtins.lt),
>                                  ("write", Builtin Builtins.write),
>                                  ("define", Builtin Builtins.defglobal),
>                                  ("apply", Apply),
>                                  ("call-with-values", CallVs),
>                                  ("call/cc", CallCC)]
>           step (e, s) (n, v) = def e s n v
>           evalPrint e s val = show <$> interpret e s (analyze val) >>= putStrLn
