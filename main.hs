import Text.ParserCombinators.Parsec
import System.Environment

-- Value Representation

data SValue = Symbol String
            | Bool Bool

-- Read

identifier :: Parser String
identifier = do
               ic <- initial
               rcs <- many subsequent
               return $ ic:rcs
             <|>
             peculiarIdentifier

initial :: Parser Char
initial = letter <|> specialInitial

specialInitial :: Parser Char
specialInitial = oneOf "!$%&*/:<=>?^_~"

subsequent :: Parser Char
subsequent = initial <|> digit <|> specialSubsequent

specialSubsequent :: Parser Char
specialSubsequent = explicitSign <|> char '.' <|> char '@'

explicitSign :: Parser Char
explicitSign = char '+' <|> char '-'

-- FIXME
peculiarIdentifier :: Parser String
peculiarIdentifier = do
                        s <- explicitSign
                        d <- char '.'
                        dsub <- dotSubsequent
                        subs <- many subsequent
                        return $ s:d:dsub:subs
                    <|> do
                          s <- explicitSign
                          ssub <- signSubsequent
                          subs <- many subsequent
                          return $ s:ssub:subs
                    <|> do
                          s <- explicitSign
                          return [s]
                    <|> do
                          d <- char '.'
                          dsub <- dotSubsequent
                          subs <- many subsequent
                          return $ d:dsub:subs

dotSubsequent :: Parser Char
dotSubsequent = signSubsequent <|> char '.'

signSubsequent :: Parser Char
signSubsequent = initial <|> explicitSign <|> char '@'

boolean :: Parser Bool
boolean = do
            char '#'
            bc <- char 't' <|> char 'f'
            case bc of
              't' -> optional (string "rue") >> return True
              'f' -> optional (string "alse") >> return False

expr :: Parser SValue
expr = Symbol <$> identifier
       <|> Bool <$> boolean

-- Write

instance Show SValue where
  show (Symbol cs) = cs
  show (Bool True) = "#t"
  show (Bool False) = "#f"

--

readExpr :: String -> String
readExpr input = case parse expr "hiss" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

main :: IO ()
main = do
         (expr:_) <- getArgs
         putStrLn (readExpr expr)
