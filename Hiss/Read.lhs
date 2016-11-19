> module Hiss.Read where
> import Data.Foldable (foldl')
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

FIXME:

> peculiarIdentifier :: Parser String
> peculiarIdentifier = do
>                         s <- explicitSign
>                         d <- char '.'
>                         dsub <- dotSubsequent
>                         subs <- many subsequent
>                         return $ s:d:dsub:subs
>                     <|> do
>                           s <- explicitSign
>                           ssub <- signSubsequent
>                           subs <- many subsequent
>                           return $ s:ssub:subs
>                     <|> do
>                           s <- explicitSign
>                           return [s]
>                     <|> do
>                           d <- char '.'
>                           dsub <- dotSubsequent
>                           subs <- many subsequent
>                           return $ d:dsub:subs

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
>             char '#'
>             bc <- char 't' <|> char 'f'
>             case bc of
>               't' -> optional (string "rue") >> return True
>               'f' -> optional (string "alse") >> return False

> parseList :: Parser SValue
> parseList = char '(' >> elems
>     where elems = (char ')' >> return Nil)
>                   <|> (char '.' >> datum <* char ')')
>                   <|> do
>                        head <- datum
>                        tail <- elems
>                        return $ Pair head tail

> simpleDatum :: Parser SValue
> simpleDatum = Bool <$> boolean
>               <|> Fixnum <$> number
>               <|> Symbol <$> identifier

> compoundDatum :: Parser SValue
> compoundDatum = parseList

> datum :: Parser SValue
> datum = between spaces spaces (simpleDatum <|> compoundDatum)
