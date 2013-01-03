-- Module to import MDS files
module Skeleton where

import Global
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Loader as L

type Bone = (Int, String, Int)
type Skeleton = [Bone]

offsetError offset cur = error $ "Offset " ++ show offset 
    ++ " not equal to " ++ show cur

parseSkel :: BLC.ByteString -> Skeleton
parseSkel s = runGet get s
  where get = do
        magic <- getWord32le
        if magic /= 0x12121212 then error $ "Bad magic1 " ++ show magic
        else do
            getWord32le -- 1
            num <- getWord32le
            magic2 <- getWord32le
            if magic2 /= 0xFFFFFFFF then error $ "Bad magic2 " ++ show magic2
            else case num of
                1 -> return []
                _ -> do
                    let r = [0..num-2]
                    originOffset <- getWord32le
                    r' <- mapM readPoint r
                    cur <- bytesRead
                    if fromIntegral originOffset /= cur 
                    then offsetError originOffset cur 
                    else do
                        originStr <- getLazyByteStringNul
                        r'' <- mapM readString r'
                        return $ r''
                            where readPoint i = do
                                    joint <- getWord32le
                                    offset <- getWord32le
                                    return (i, offset, joint)
                            where readString (i, offset, joint) = do
                                    cur <- bytesRead
                                    if fromIntegral offset /= cur 
                                    then offsetError offset cur
                                    else do
                                        str <- getLazyByteStringNul
                                        return (fromIntegral i, 
                                            BLC.unpack str, fromIntegral joint)

parseFile :: FileName -> IO String
parseFile path = do
    fileMap <- L.fullFileMap
    fileStr <- L.readPath fileMap path
    let js = parseSkel fileStr
    return $ show js
            



