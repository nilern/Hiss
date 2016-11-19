> import Text.ParserCombinators.Parsec
> import System.Environment
> import Hiss.Data
> import Hiss.Read (datum)
> import Hiss.Analyze (analyze)

> main :: IO ()
> main = do
>          (expr:_) <- getArgs
>          case parse datum "hiss" expr of
>            Left err -> putStrLn $ "No match: " ++ show err
>            Right val ->
>              putStrLn $ "Found value " ++ show (analyze val)
