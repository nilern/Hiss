> import Text.ParserCombinators.Parsec (parse)
> import System.Environment (getArgs)
> import Hiss.Data (emptyEnv)
> import Hiss.Read (datum)
> import Hiss.Analyze (analyze)
> import Hiss.Interpret (interpret)

> main :: IO ()
> main = do
>          (expr:_) <- getArgs
>          case parse datum "hiss" expr of
>            Left err -> putStrLn $ show err
>            Right val ->
>              putStrLn $ show $ interpret emptyEnv $ analyze val
