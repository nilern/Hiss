
= Interpreter Entry Point

All this does is read the bootstrap Scheme file, wrap the statements found
therein in a `begin` and then pipe the resulting form through analysis and
interpretation, either succeeding with a value or failing with an error message.

In addition to implementing a fuller Scheme on top of the restricted one
provided by the Haskell code the `__bootstrap__.scm` file also parses the
command line arguments, finds the user code and runs it.

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
