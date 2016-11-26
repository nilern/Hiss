> import Text.ParserCombinators.Parsec (parse)
> import System.Environment (getArgs)
> import Hiss.Data (SValue(Symbol, CallCC, CallVs, Apply),
>                   toplevelFromList, emptyStore, injectList)
> import Hiss.Read (datums)
> import Hiss.Analyze (analyze)
> import Hiss.Interpret (interpret)

> main :: IO ()
> main = do
>          args <- getArgs
>          expr <- case args of
>                    [filename] -> readFile filename
>                    ["-e", expr] -> return expr
>          case parse datums "hiss" expr of
>            Left err -> putStrLn $ show err
>            Right vals -> do eg <- initToplevel
>                             evalPrint eg emptyStore
>                               $ injectList (Symbol "##sf#begin" : vals)
>     where initToplevel = toplevelFromList
>                              [("apply", Apply),
>                               ("call-with-values", CallVs),
>                               ("call/cc", CallCC)]
>           evalPrint e s val = show <$> interpret e s (analyze val) >>= putStrLn
