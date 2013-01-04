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
import qualified Data.Char as C

lowerCase s = map C.toLower s

gamePath = "/Applications/Games/Din's Curse/Assets/"

type DirName = String
type FileName = String
type ZipFilePath = String

-- (lowercaseName, (regularName, Maybe ZipPath)
data Directory = Directory DirName (M.Map String (FileName, Maybe FilePath)) [Directory]
    deriving Show

instance Eq Directory where
    (Directory a _ _) == (Directory b _ _) = lowerCase a == lowerCase b

instance Ord (Directory) where
    compare (Directory a _ _) (Directory b _ _) = compare (lowerCase a) (lowerCase b)

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
                              where newFM = M.insert (lowerCase file) (file, value) fileMap
        (newDir, rest) -> 
            let (d, notDs) = L.partition (\(Directory n _ _) -> n == newDir) ds
            in case d of
                []   -> let newD = Directory newDir M.empty [] in
                        Directory n fileMap $ L.insert (addToDir newD (rest, value)) ds
                [d'] -> Directory n fileMap $ L.insert (addToDir d' (rest, value)) notDs
                _    -> error $ "Too many directories " ++ newDir ++ " found"

subdir (Directory _ _ ds) name = L.find (\(Directory n _ _) -> lowerCase n == lowerCase name) ds

-- return info about file/dir. We return the Directory & proper case name
data PathType = DirPathType Directory FilePath
               | FilePathType Directory FilePath 
               | ZipPathType Directory FilePath ZipFilePath
               | NotFoundPathType
               deriving Show

findPath :: Directory -> FileName -> PathType
findPath d path = inner d path []
  where inner d@(Directory n _ _) [] accPath  = DirPathType d $ join accPath n
        inner d@(Directory n fileMap ds) path accP =
          let accPath = join accP n
          in case splitPath path of
            ([], _)      -> error $ "findPath: Error splitting the path " ++ path
            (name, rest) -> case M.lookup (lowerCase name) fileMap of
                              Just (file, Just v)  -> ZipPathType d (join accPath file) v
                              Just (file, Nothing) -> FilePathType d $ join accPath file
                              Nothing -> case subdir d name of 
                                                Just dir -> inner dir rest accPath
                                                Nothing  -> NotFoundPathType
        join "" y = y
        join x y = x ++ "/" ++ y

readPath d path = case findPath d path of
        ZipPathType _ casePath zpath -> do
            bytes <- withArchive [] (gamePath ++ zpath) $ fileContents [] casePath
            let bytes' = B.pack bytes
            return bytes'
        FilePathType _ casePath -> do
            bytes <- B.readFile $ gamePath ++ casePath
            return bytes
        DirPathType _ _  -> ioError $ userError $ "readFile: can't read directory " ++ path
        NotFoundPathType -> ioError $ userError $ "readFile: file " ++ path ++ " not found"

directoryMapOfFileList fl = L.foldl' addToDir (Directory "" M.empty []) fl

fullFileMap :: IO Directory
fullFileMap = do
    list <- fullFileList
    return $ directoryMapOfFileList list

