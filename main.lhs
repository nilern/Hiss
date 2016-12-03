> import Text.ParserCombinators.Parsec (parse)
> import Text.Parsec.Pos (initialPos)
> import System.Environment (getArgs)
> import Hiss.Data (SValue(Symbol, Pair, Syntax), emptyEnv)
> import Hiss.Read (datums)
> import Hiss.Analyze (analyze)
> import Hiss.Interpret (interpret)

> main :: IO ()
> main = do
>          args <- getArgs
>          (expr, filename) <- case args of
>                                [filename] -> do expr <- readFile filename
>                                                 return (expr, filename)
>                                ["-e", expr] -> return (expr, "__CLI_ARG__")
>          case parse datums filename expr of
>            Left err -> putStrLn $ show err
>            Right vals -> do env <- emptyEnv
>                             evalPrint filename env (blockify vals)
>     where blockify (Syntax exprs ctx pos) =
>               Syntax (Pair (Syntax (Symbol "begin") ctx pos)
>                            exprs) ctx pos
>           evalPrint filename e val =
>               case analyze val of
>                 Right ast ->
>                     do ev <- interpret (initialPos filename) e ast
>                        case ev of
>                          Right v -> putStrLn $ show v
>                          Left err -> putStrLn $ "Error: " ++ show err
>                 Left err -> putStrLn $ "Error: " ++ show err
