> import Text.ParserCombinators.Parsec
> import System.Environment
> import Hiss.Data
> import Hiss.Read (datum)
> import Hiss.Write

> readDatum :: String -> String
> readDatum input = case parse datum "hiss" input of
>     Left err -> "No match: " ++ show err
>     Right val -> "Found value " ++ show val

> main :: IO ()
> main = do
>          (expr:_) <- getArgs
>          putStrLn (readDatum expr)
