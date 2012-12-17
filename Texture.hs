module Texture (
    loadGLTexture
    ) where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI
import Data.Binary
import Data.Binary.Get

data TextureFormat = DXT1 | DXT5
data CompressedTexture = CompressedTexture {
                        width::Int, 
                        height::Int, 
                        imageType::Int, 
                        mipMaps::Int, 
                        size::Int,
                        format::TextureFormat,
                        offset::Int,
                        ptr::ForeignPtr Word8}

instance Show Texture where
    show t = "width: " ++ show (width t) ++
            " height: " ++ show (height t) ++
            " offset: " ++ show (offset t) ++
            " format: " ++ show (format t) ++
            " mipmaps: " ++ show (mipMaps t) ++
            " size: " ++ show (size t)

instance Binary Texture where
    put t = do put (0 :: Word8) -- not used
    get = do w <- getWord32le
             h <- getWord32le
             imageType <- getWord32le
             mipMaps <- getWord32le
             formatStr <- getByteString 4
             let format = case (imageType, B.unpack format) of
                3, "DXT1" -> DXT1
                4, "DXT5" -> DXT5
                i, x -> error "Unrecognized format " ++ show i ++ ", " ++ x
             s <- getRemainingLazyByteString
             let s' = B.concat $ BL.toChunks s
                 (p,offset,size) = BI.toForeignPtr s'
             return $ Texture {width=fromIntegral w, 
                                  height=fromIntegral h, 
                                  imageType=fromIntegral x, 
                                  mipMaps=fromIntegral y, 
                                  size=size, 
                                  format=format,
                                  offset=offset, 
                                  ptr=p}

readTexture :: BL.ByteString -> Texture
readTexture file = decode file 

-- load a texture
loadGLTexture fileStr "CTX" = loadCompressedTexture fileStr
loadGLTexture fileStr "TGA" = loadUncompressedTexture fileStr
loadGLTexture fileStr ext   = ioError $ userError 
    $ "Don't know how to read " ++ ext ++ " file"

loadUncompressedTexture fileStr = do
  texName <- liftM head (genObjectNames 1)
  GL.textureBinding GL.Texture2D $= Just texName
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GLFW.loadMemoryTexture2D (B.unpack fileStr) []
  return texName

loadCompressedTexture fileStr = do
  let tex = decode fileStr
  putStrLn $ show t -- debug
  texName <- liftM head (genObjectNames 1)
  textureBinding Texture2D $= Just texName
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

  let (pixelFormat,blockSize) = case (format tex) of
    DXT1 -> (GL.CompressedTextureFormat gl_COMPRESSED_RGBA_S3TC_DXT1, 8)
    DXT3 -> (GL.CompressedTextureFormat gl_COMPRESSED_RGBA_S3TC_DXT3, 16)
    DXT5 -> (GL.CompressedTextureFormat gl_COMPRESSED_RGBA_S3TC_DXT5, 16)
  
  withForeignPtr (ptr tex) $ \p -> do
    let size = (width tex) * (height tex) * blockSize
        adjustedPtr = p `plusPtr` (offset tex)
        c = GL.CompressedPixelData pixelFormat (fromIntegral size) adjustedPtr
    compressedTexImage2D Nothing NoProxy 
        0 (TextureSize2D (fromIntegral $ width t) (fromIntegral $ height t)) 0 c
  return texName

{-calcMipMaps :: CompressedTexture -> [(Int, Int)] -- offset, size-}
{-calcMipMaps -}
