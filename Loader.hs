{-# LANGUAGE DeriveDataTypeable #-}

module Loader (
        -- types
          DirName
        , FileName
        , Directory

        -- methods
        , fullFileMap
        , findPath
        , readPath
       ) where

import System.Directory(getDirectoryContents)
import qualified Data.ByteString.Lazy as B
import Codec.Archive.LibZip
import qualified Data.List as L
import qualified Data.Map as M

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
    files <- withArchive [] n $ fileNames []
    return files

filesInZipPerFile n = do
    let fullPath = gamePath ++ n
    files <- filesInZip fullPath
    let files' = filter isFile files
    let l = map (\x -> (x, Just n)) files'
    return l

isZip name = let suffix = drop (length name - length ".zip") name
    in suffix == ".zip"

isHidden (n:ns) = (n == '.')
isHidden []     = True

-- No other way to tell it's a file unfortunately
isFile name = let suffix = drop (length name - length ".***") name
    in head suffix == '.'

-- Order is important here. We sort so that the highest alphanumeric values (file-wise)
-- are last, and the non-zip files are after that, so they take highest precedence
fullFileList = do
    dirContents <- getDirectoryContents gamePath
    let zipFiles = L.sort $ filter isZip dirContents
    zipFiles' <- mapM filesInZipPerFile zipFiles
    {-zipFiles' <- mapM (wrapMaybeIO filesInZip . (gamePath++)) ["Assets002.zip"]-}
    let nonZipFiles = filter (not . isHidden) $ filter (not . isZip) dirContents
    let nonZipFiles' = map (\f -> (f, Nothing)) nonZipFiles 
    return $ (concat zipFiles') ++ nonZipFiles' 

splitPath ('/':xs) = splitPath xs
splitPath path     = case L.elemIndex '/' path of
    Nothing -> (path, [])
    Just i  -> let (first, last) = splitAt i path
               in case last of
                   '/':xs -> (first, xs)
                   _      -> (first, last)

addToDir (Directory n fileMap ds) (path, value) = 
    case splitPath path of
        ([],   _)      -> error $ "addToDir: Error splitting the path " ++ path
        (file, [])     -> Directory n newFM ds
                              where newFM = M.insert file value fileMap
        (newDir, rest) -> 
            let (d, notDs) = L.partition (\(Directory n _ _) -> n == newDir) ds
            in case d of
                []   -> let newD = Directory newDir M.empty [] in
                        Directory n fileMap $ L.insert (addToDir newD (rest, value)) ds
                [d'] -> Directory n fileMap $ L.insert (addToDir d' (rest, value)) notDs
                _    -> error $ "Too many directories " ++ newDir ++ " found"

subdir (Directory _ _ ds) name = L.find (\(Directory n _ _) -> n == name) ds

-- return info about file
data PathType = DirPathType Directory
               | FilePathType Directory
               | ZipPathType Directory FilePath
               | NotFoundPathType

findPath :: Directory -> FileName -> PathType
findPath d [] = DirPathType d -- return directory only
findPath d@(Directory n fileMap ds) path =
    case splitPath path of
        ([], _)      -> error $ "findPath: Error splitting the path " ++ path
        (name, rest) -> case M.lookup name fileMap of
                            Just (Just v)  -> ZipPathType d v -- found the file, return the archive
                            Just Nothing   -> FilePathType d
                            Nothing        -> case subdir d name of 
                                                Just dir -> findPath dir rest
                                                Nothing  -> NotFoundPathType

{-readPath :: Directory -> String -> IO String-}
readPath d path = case findPath d path of
        ZipPathType _ zpath -> do
            bytes <- withArchive [] (gamePath ++ zpath) $ fileContents [] path
            let bytes' = B.pack bytes
            return bytes'
        FilePathType _ -> do
            bytes <- B.readFile $ gamePath ++ path
            return bytes
        DirPathType _    -> ioError $ userError $ "readFile: can't read directory " ++ path
        NotFoundPathType -> ioError $ userError $ "readFile: file " ++ path ++ " not found"

directoryMapOfFileList fl = L.foldl' addToDir (Directory "." M.empty []) fl

{-fullFileMap :: IO (M.Map String (Maybe FilePath))-}
{-fullFileMap = do-}
    {-list <- fullFileList-}
    {-return $ M.fromList list-}

fullFileMap :: IO Directory
fullFileMap = do
    list <- fullFileList
    return $ directoryMapOfFileList list

