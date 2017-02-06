
= Scheme Reader

This is a rather straightforward rendition into Parsec of the grammar found in
chapter 7 of the R7RS.

> module Hiss.Read (datums) where
> import Text.ParserCombinators.Parsec
> import Hiss.Data

> identifier :: Parser String
> identifier = standardIdentifier <|> internalIdentifier

> standardIdentifier :: Parser String
> standardIdentifier = do ic <- initial
>                         rcs <- many subsequent
>                         return $ ic:rcs
>                      <|> peculiarIdentifier

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

> internalIdentifier :: Parser String
> internalIdentifier = do start <- string "##"
>                         ns <- many1 letter
>                         split <- string "#"
>                         rest <- standardIdentifier
>                         return $ start ++ ns ++ split ++ rest

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

> parseString :: Parser String
> parseString = between (char '"') (char '"') (many stringElement)
>     where stringElement = noneOf ['"', '\\']

> parseList :: Parser SValue
> parseList = char '(' >> elems
>     where elems = (char ')' >> return Nil)
>                   <|> (char '.' >> datum <* char ')')
>                   <|> do
>                        x <- datum
>                        xs <- elems
>                        return $ Pair x xs

> simpleDatum :: Parser SValue
> simpleDatum = Bool <$> try boolean
>               <|> Fixnum <$> number
>               <|> Symbol <$> identifier
>               <|> String <$> parseString

> compoundDatum :: Parser SValue
> compoundDatum = parseList <|> abbreviation

> abbreviation :: Parser SValue
> abbreviation = do pos <- getPosition
>                   _ <- char '\''
>                   d <- datum
>                   return $ Pair (Syntax (Symbol "quote") mempty pos)
>                                 (Pair d Nil)

> atmosphere :: Parser String
> atmosphere = (many1 space) <|> comment

> comment :: Parser String
> comment = do c <- char ';'
>              cs <- manyTill anyChar $ try $ char '\n'
>              return $ c:cs

> datum :: Parser SValue
> datum = between (many atmosphere) (many atmosphere) solid
>     where solid = do pos <- getPosition
>                      content <- simpleDatum <|> compoundDatum
>                      return $ Syntax content mempty pos

> datums :: Parser SValue
> datums = do pos <- getPosition
>             ds <- manyTill datum eof
>             return $ Syntax (injectList ds) mempty pos
