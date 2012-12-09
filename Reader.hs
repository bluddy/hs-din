module Reader where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI
import Data.Binary
import Data.Binary.Get
import qualified Texture as T

instance Binary T.Texture where
    put t = do put (0 :: Word8) -- not used
    get = do w <- getWord32le
             h <- getWord32le
             x <- getWord32le
             y <- getWord32le
             s <- getRemainingLazyByteString
             let s' = B.concat $ BL.toChunks s
                 (p,offset,size) = BI.toForeignPtr s'
             return $ T.Texture {T.width=fromIntegral w, 
                                  T.height=fromIntegral h, 
                                  T.unknownX=fromIntegral x, 
                                  T.unknownY=fromIntegral y, 
                                  T.size=size, 
                                  T.offset=offset, 
                                  T.ptr=p}

readTexture :: BL.ByteString -> T.Texture
readTexture file = decode file 





        
