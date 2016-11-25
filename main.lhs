> import Text.ParserCombinators.Parsec (parse)
> import System.Environment (getArgs)
> import Hiss.Data (SValue(Symbol, PureBuiltin, Builtin, CallCC, CallVs, Values,
>                          Apply),
>                   toplevelFromList, emptyStore, injectList)
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
>            Right vals -> do eg <- initToplevel
>                             evalPrint eg emptyStore $ injectList (Symbol "begin" : vals)
>     where initToplevel = toplevelFromList
>                              [("+", PureBuiltin Builtins.add),
>                               ("-", PureBuiltin Builtins.sub),
>                               ("*", PureBuiltin Builtins.mul),
>                               ("<", PureBuiltin Builtins.lt),
>                               ("write", Builtin Builtins.write),
>                               ("define", Builtin Builtins.defglobal),
>                               ("apply", Apply),
>                               ("call-with-values", CallVs),
>                               ("values", Values),
>                               ("call/cc", CallCC)]
>           evalPrint e s val = show <$> interpret e s (analyze val) >>= putStrLn
