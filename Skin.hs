module Skin where

import Global
import Text.Parsec
import Text.Parsec.String
import qualified Parser as P
import qualified Loader as L
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Applicative hiding ((<|>), many, optional)

data Surface = Surface Int FileName deriving Show
data Skin = Skin Directory [Surface] deriving Show

parseSkin = do
    P.n
    dir <- P.strC "dir" *> P.s *> P.filePath <* P.n
    surfaces <- many1 parseSurface
    return $ Skin dir surfaces

parseSurface = do
    int <- P.strC "surface" *> P.s *> P.integer <* P.s
    path <- P.filePath <* P.n
    return $ Surface int path

doParse :: String -> Skin
doParse text = 
    let s = P.uncomment text
    in case parse parseSkin "?" s of
      Left err -> error $ show err
      Right m -> m

parseFile :: FileName -> IO String
parseFile path = do
    fileMap <- L.fullFileMap
    fileStr <- L.readPath fileMap path
    let model = doParse (BLC.unpack fileStr)
    return $ show model

