-- Module to import MDS files
module Skeleton where

import Global
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Loader as L

type Bone = (Int, String, [Int])

offsetError offset cur = error $ "Offset " ++ show offset 
    ++ " not equal to " ++ show cur

parseJoints :: BLC.ByteString -> [(Int, String, Int)]
parseJoints s = runGet get s
  where get = do
        magic <- getWord32le
        if magic /= 0x12121212 then error $ "Bad magic1 " ++ show magic
        else do
            getWord32le -- 1
            num <- getWord32le
            magic2 <- getWord32le
            if magic2 /= 0xFFFFFFFF then error $ "Bad magic2 " ++ show magic2
            else case num of
                1 -> do
                    offset <- getWord32le
                    cur <- bytesRead
                    if fromIntegral offset /= cur then offsetError offset cur
                    else do
                        str <- getLazyByteStringNul
                        return [(0, BLC.unpack str, -1)]
                _ -> do
                    let r = [1..num-1]
                    originOffset <- getWord32le
                    r' <- mapM readPoint r
                    cur <- bytesRead
                    if fromIntegral originOffset /= cur 
                    then offsetError originOffset cur 
                    else do
                        originStr <- getLazyByteStringNul
                        r'' <- mapM readString r'
                        return $ (0,BLC.unpack originStr,(-1)):r''
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

jointsToBones :: [(Int, String, Int)] -> [Bone]
jointsToBones js = map toBone js
    where toBone (bone, str, joint) = 
            let touching = filter (\(b,_,j) -> j == joint && b /= bone) js
                touching' = map (\(b,_,_) -> b) touching
            in (bone, str, touching')
        
parseFile :: Filename -> IO String
parseFile path = do
    fileMap <- L.fullFileMap
    fileStr <- L.readPath fileMap path
    let js = parseJoints fileStr
        bs = jointsToBones js
    return $ show bs
            



