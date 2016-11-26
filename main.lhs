> import Text.ParserCombinators.Parsec (parse)
> import System.Environment (getArgs)
> import Hiss.Data (SValue(Symbol, Pair, Syntax),
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
>                             evalPrint eg emptyStore $ blockify vals
>     where initToplevel = toplevelFromList []
>           blockify (Syntax exprs ctx pos) =
>               Syntax (Pair (Syntax (Symbol "##sf#begin") ctx pos)
>                            exprs) ctx pos
>           evalPrint e s val = show <$> ast >>= putStrLn
>               where ast = interpret e s $ analyze val
