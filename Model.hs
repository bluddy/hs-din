module Model where

import Text.Parsec
import Text.Parsec.String
import qualified Animation as A
import qualified Loader as L
import Data.Functor
import Control.Applicative hiding ((<|>), many, optional)
import qualified Data.List as L

type Directory = String
type FileName = String

data Model = Model {dir::Directory,
              skeleton::FileName,
              mesh::FileName,
              tags::Maybe FileName,
              skin::FileName,
              objectType::String,
              animations::[A.Animation]
              } deriving Show

{-parseFile file = do-}
    {-filemap <- L.fullFileMap-}
    {-fileStr <- L.readPath filemap file-}

filePath :: Parser String
filePath = many1 $ alphaNum <|> char '/'

parsePath :: String -> Parser String
parsePath text = string text *> spaces *> filePath <* spaces

parseModel :: Parser Model
parseModel = do 
    let model = Model "" "" "" Nothing "" "" []
    loop model 
        where loop a = do
                a' <- choice [
                        do {x <- parsePath "dir"; return a {dir=x}},
                        do {x <- parsePath "skeleton"; return a {skeleton=x}},
                        do {x <- parsePath "mesh"; return a {mesh=x}},
                        do {x <- parsePath "tags"; return a {tags=Just x}},
                        do {x <- parsePath "skin"; return a {skin=x}},
                        do {x <- parsePath "objectType"; return a {objectType=x}}
                      ]
                isEof <- option False (eof *> return True)
                if isEof then return a'
                else loop a'

doParse :: String -> Model
doParse file = let s = concat $ L.intersperse " " $ lines $ file
    in case parse parseModel "?" s of
      Left err -> error $ show err
      Right m -> m
    
    
             
    
    


