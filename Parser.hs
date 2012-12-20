module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.Functor ((<$>))
import Control.Applicative hiding ((<|>), many, optional)

data Comment = LineComment
             | MultiLineComment
             | NoComment
             deriving Eq
              
uncomment :: String -> String
uncomment s = inner s NoComment
    where inner ('/':'/':xs) NoComment = inner xs LineComment
          inner ('/':'*':xs) NoComment = inner xs MultiLineComment
          inner ('\n':xs) LineComment = inner xs NoComment
          inner ('*':'/':xs) MultiLineComment = inner xs NoComment
          inner (x:xs) c@LineComment = inner xs c
          inner (x:xs) c@MultiLineComment = inner xs c
          inner (x:xs) c@NoComment = x:inner xs c
          inner [] _ = []

n :: Parser String
n = many (char ' ' <|> char '\n' <|> char '\r' <* char '\n' <|> char '\t')

s :: Parser String
s = many (char ' ' <|> char '\t')

filePath :: Parser String
filePath = many1 $ alphaNum <|> char '/' <|> char '.' <|> char '_' <|> char '-'

name :: Parser String
name = many1 $ alphaNum <|> char '_'

integer :: Parser Int
integer = read <$> (many1 $ digit)

