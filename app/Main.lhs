> {-# LANGUAGE ScopedTypeVariables #-}

> import System.Environment (getArgs)
> import Control.Eff
> import Control.Eff.Exception
> import Text.ParserCombinators.Parsec (parse)
> import Text.Parsec.Pos (initialPos)
> import Hiss.Data (SValue(Symbol, Pair, Syntax), SError, emptyEnv)
> import Hiss.Read (datums)
> import Hiss.Analyze (analyze)
> import Hiss.Interpret (interpret)

> main :: IO ()
> main = do exprStr <- readFile bsFile
>           case parse datums bsFile exprStr of
>             Left err -> putStrLn $ show err
>             Right vals -> do env <- emptyEnv
>                              eval bsFile env (blockify vals)
>     where bsFile = "__bootstrap__.scm"
>           blockify (Syntax exprs ctx pos) =
>               Syntax (Pair (Syntax (Symbol "begin") ctx pos)
>                            exprs) ctx pos
>           eval filename e val =
>               case run $ runExc $ analyze val of
>                 Right ast ->
>                     do ev <- interpret (initialPos filename) e ast
>                        case ev of
>                          Right v -> return ()
>                          Left err -> putStrLn $ "Error: " ++ show err
>                 Left (err :: SError) -> putStrLn $ "Error: " ++ show err
