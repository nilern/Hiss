> module Hiss.Read where
> import Text.ParserCombinators.Parsec
> import Hiss.Data

= Read

> identifier :: Parser String
> identifier = do
>                ic <- initial
>                rcs <- many subsequent
>                return $ ic:rcs
>              <|>
>              peculiarIdentifier

> initial :: Parser Char
> initial = letter <|> specialInitial

> specialInitial :: Parser Char
> specialInitial = oneOf "!$%&*/:<=>?^_~"

> subsequent :: Parser Char
> subsequent = initial <|> digit <|> specialSubsequent

> specialSubsequent :: Parser Char
> specialSubsequent = explicitSign <|> char '.' <|> char '@'

> explicitSign :: Parser Char
> explicitSign = char '+' <|> char '-'

> peculiarIdentifier :: Parser String
> peculiarIdentifier = do
>                        os <- optionMaybe explicitSign
>                        case os of
>                          Just s -> do
>                                      d <- char '.'
>                                      dsub <- dotSubsequent
>                                      subs <- many subsequent
>                                      return $ s:d:dsub:subs
>                                    <|> do
>                                          ssub <- signSubsequent
>                                          subs <- many subsequent
>                                          return $ s:ssub:subs
>                                    <|> return [s]
>                          Nothing -> do
>                                       d <- char '.'
>                                       dsub <- dotSubsequent
>                                       subs <- many subsequent
>                                       return $ d:dsub:subs

> dotSubsequent :: Parser Char
> dotSubsequent = signSubsequent <|> char '.'

> signSubsequent :: Parser Char
> signSubsequent = initial <|> explicitSign <|> char '@'

> number :: Parser Int
> number = do
>            ds <- many1 digit
>            return $ read ds

> boolean :: Parser Bool
> boolean = do
>             _ <- char '#'
>             (char 't' >> optional (string "rue") >> return True)
>              <|> (char 'f' >> optional (string "alse") >> return False)

> parseList :: Parser SValue
> parseList = char '(' >> elems
>     where elems = (char ')' >> return Nil)
>                   <|> (char '.' >> datum <* char ')')
>                   <|> do
>                        x <- datum
>                        xs <- elems
>                        return $ Pair x xs

> simpleDatum :: Parser SValue
> simpleDatum = Bool <$> boolean
>               <|> Fixnum <$> number
>               <|> Symbol <$> identifier

> compoundDatum :: Parser SValue
> compoundDatum = parseList <|> abbreviation

> abbreviation :: Parser SValue
> abbreviation = do char '\''
>                   d <- datum
>                   return $ Pair (Symbol "quote") (Pair d Nil)

> datum :: Parser SValue
> datum = between spaces spaces (simpleDatum <|> compoundDatum)

> datums :: Parser [SValue]
> datums = manyTill datum eof
