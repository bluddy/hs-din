module Loader where

import System.Directory(getDirectoryContents)
import Codec.Archive.Zip(toArchive, filesInArchive)
import qualified Data.List as L
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Control.Applicative ((<$>))

gamePath = "/Applications/Games/Din's Curse/Assets/"

type DirName = String
type FileName = String
data Directory = Directory DirName (M.Map FileName (Maybe FilePath)) [Directory]
    deriving Show

instance Eq Directory where
    (Directory a _ _) == (Directory b _ _) = a == b

instance Ord (Directory) where
    compare (Directory a _ _) (Directory b _ _) = compare a b

filesInZip n = do
    archive <- toArchive <$> B.readFile n
    let files = filesInArchive archive
    return files

wrapMaybeIO :: (a -> IO [b]) -> a -> IO [(b, Maybe a)]
wrapMaybeIO f n = do
    r <- f n
    let l = map (\x -> (x, Just n)) r
    return l

isZip name = let suffix = drop (length name - length ".zip") name
    in suffix == ".zip"

isHidden (n:ns) = (n == '.')
isHidden []     = True

-- Order is important here. We sort so that the highest alphanumeric values (file-wise)
-- are last, and the non-zip files are after that, so they take highest precedence
fullFileList = do
    dirContents <- getDirectoryContents gamePath
    let zipFiles = L.sort $ filter isZip dirContents 
    {-zipFiles' <- mapM (wrapMaybeIO filesInZip . (gamePath++)) zipFiles-}
    zipFiles' <- mapM (wrapMaybeIO filesInZip . (gamePath++)) ["assets001.zip"]
    let nonZipFiles = filter (not . isHidden) $ filter (not . isZip) dirContents
    let nonZipFiles' = map (\f -> (f, Nothing)) nonZipFiles 
    return $ (concat zipFiles') ++ nonZipFiles' 

splitPath ('/':xs) = splitPath xs
splitPath path     = case L.elemIndex '/' path of
    Nothing -> (path, [])
    Just i  -> splitAt i path

addToDir (Directory n fileMap ds) (path, value) = 
    case splitPath path of
        ([],   _)      -> error $ "Error splitting the path " ++ path
        (file, [])     -> Directory n newFM ds
                              where newFM = M.insert file value fileMap
        (newDir, last) -> 
            let (d, notDs) = L.partition (\(Directory n _ _) -> n == newDir) ds
            in case d of
                []   -> let newD = Directory newDir M.empty [] in
                        Directory n fileMap $ L.insert (addToDir newD (last, value)) ds
                [d'] -> Directory n fileMap $ L.insert (addToDir d' (last, value)) notDs
                _    -> error $ "Too many directories " ++ newDir ++ " found"

directoryMapOfFileList fl = L.foldl' addToDir (Directory "." M.empty []) fl

{-fullFileMap :: IO (M.Map String (Maybe FilePath))-}
{-fullFileMap = do-}
    {-list <- fullFileList-}
    {-return $ M.fromList list-}

fullFileMap :: IO Directory
fullFileMap = do
    list <- fullFileList
    return $ directoryMapOfFileList list

