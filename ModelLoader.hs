module ModelLoader where

import Global
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Perm
import Data.Functor ((<$>))
import Data.Char (toLower)
import Control.Applicative hiding ((<|>), many, optional)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.List as List
import qualified Animation as A
import qualified Loader as L
import qualified Parser as P

data Model = Model {dir::Directory,
              skel::FileName,
              mesh::FileName,
              tags::Maybe FileName,
              skin::FileName,
              objectType::Maybe String,
              anims::[A.Animation]
              } deriving Show

parsePath :: String -> Parser String
parsePath text = P.strC text *> P.s *> P.filePath

parseAnims :: Parser [A.Animation]
parseAnims = many parseA
               where parseA = do
                         a <- A.parseAnimation
                         return a

parseModel :: Parser Model
parseModel = do 
    P.n
    permute $ Model 
            <$$> parsePath "dir" <* P.n
            <||> try (parsePath "skeleton" <* P.n)
            <||> parsePath "mesh" <* P.n
            <|?> (Nothing, Just <$> parsePath "tags" <* P.n)
            <||> try (parsePath "skin" <* P.n)
            <|?> (Nothing, Just <$> parsePath "objecttype" <* P.n)
            <||> do P.strC "animations" *> P.n *> char '{' *> P.n
                    a <- parseAnims
                    P.n *> char '}'
                    return a

doParse :: String -> Model
doParse text = 
    let s = P.uncomment text
    in case parse parseModel "?" s of
      Left err -> error $ show err
      Right m -> m
    
parseFile :: FileName -> IO String
parseFile path = do
    fileMap <- L.fullFileMap
    fileStr <- L.readPath fileMap path
    let model = doParse (BLC.unpack fileStr)
    return $ show model
             
    
    


