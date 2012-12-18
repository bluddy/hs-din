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

spaces1 :: Parser String
spaces1 = many1 space

{-eol =    try (string "\n\r")-}
    {-<|>  try (string "\r\n")-}
    {-<|>  string "\n"-}
    {-<|>  string "\r"-}
    {-<?>  "end of line"-}

parsePath :: String -> Parse String
parsePath text = spaces *> string text *> spaces *> filePath

parseModel :: Parser Model
parseModel = do 
    let a' = Model "" "" "" Nothing "" "" []
    dir <- parsePath "dir" 
    let a = a' {dir=dir}
    skeleton <- parsePath "skeleton"
    let b = a {skeleton=skeleton}
    mesh <- parsePath "mesh"
    let c = b {mesh=mesh}
    skin <- parsePath "skin"
    let d = c {skin=skin}
    tags <- optionMaybe $ parsePath "tags"
    let e = d {tags=tags}
    animations <- parseAnimations
    let f = e {animations=animations}
    return f

doParse :: String -> Model
doParse file = let s = concat $ L.intersperse " " $ lines $ file
    in case parse parseModel "?" s of
      Left err -> error "Failed to parse"
      Right m -> m
    
    
             
    
    


