> import Text.ParserCombinators.Parsec (parse)
> import System.Environment (getArgs)
> import Hiss.Data (SValue(Symbol), toplevelFromList, emptyStore, injectList)
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
>     where initToplevel = toplevelFromList []
>           evalPrint e s val = show <$> interpret e s (analyze val) >>= putStrLn
