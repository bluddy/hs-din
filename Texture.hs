module Texture (
    loadGLTexture
    ) where

import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI
import Data.Binary
import Data.Binary.Get
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw.EXT.TextureCompressionS3tc as GLX
import Control.Monad

data TextureFormat = DXT1 | DXT3 | DXT5 deriving Show
data CompressedTexture = CompressedTexture {
                        width::Int, 
                        height::Int, 
                        imageType::Int, 
                        mipMaps::Int, 
                        size::Int,
                        format::TextureFormat,
                        offset::Int,
                        ptr::ForeignPtr Word8}

instance Show CompressedTexture where
    show t = "width: " ++ show (width t) ++
            " height: " ++ show (height t) ++
            " offset: " ++ show (offset t) ++
            " format: " ++ show (format t) ++
            " mipmaps: " ++ show (mipMaps t) ++
            " size: " ++ show (size t)

instance Binary CompressedTexture where
    put t = do put (0 :: Word8) -- not used
    get = do w <- getWord32le
             h <- getWord32le
             imageType <- getWord32le
             mipMaps <- getWord32le
             formatStr <- getByteString 4
             let format = case (imageType, BC.unpack formatStr) of
                           (3, "DXT1") -> DXT1
                           (4, "DXT5") -> DXT5
                           (i, x) -> error $ "Unrecognized format " ++ show i ++ ", " ++ x
             s <- getRemainingLazyByteString
             let s' = B.concat $ BL.toChunks s
                 (p,offset,size) = BI.toForeignPtr s'
             return $ CompressedTexture {width=fromIntegral w, 
                                  height=fromIntegral h, 
                                  imageType=fromIntegral imageType, 
                                  mipMaps=fromIntegral mipMaps, 
                                  size=size, 
                                  format=format,
                                  offset=offset, 
                                  ptr=p}

-- load a texture
loadGLTexture fileStr "CTX" = loadCompressedTexture fileStr
loadGLTexture fileStr "TGA" = loadUncompressedTexture fileStr
loadGLTexture fileStr ext   = ioError $ userError 
    $ "Don't know how to read " ++ ext ++ " file"

loadUncompressedTexture fileStr = do
  texName <- liftM head (genObjectNames 1)
  GL.textureBinding GL.Texture2D $= Just texName
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GLFW.loadMemoryTexture2D (BLC.unpack fileStr) []
  return texName

loadCompressedTexture fileStr = do
  let tex = decode fileStr
  putStrLn $ show tex -- debug
  texName <- liftM head (GL.genObjectNames 1)
  GL.textureBinding Texture2D $= Just texName
  GL.textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  let (pixelFormat,blockSize) = case (format tex) of
       DXT1 -> (GL.CompressedTextureFormat GLX.gl_COMPRESSED_RGBA_S3TC_DXT1, 8)
       DXT3 -> (GL.CompressedTextureFormat GLX.gl_COMPRESSED_RGBA_S3TC_DXT3, 16)
       DXT5 -> (GL.CompressedTextureFormat GLX.gl_COMPRESSED_RGBA_S3TC_DXT5, 16)
  withForeignPtr (ptr tex) $ \p -> do
    let calc x = (x + 3) `div` 4
        size = (calc $ width tex) * (calc $ height tex) * blockSize
        adjustedPtr = p `plusPtr` (offset tex)
        c = GL.CompressedPixelData pixelFormat (fromIntegral size) adjustedPtr
    compressedTexImage2D Nothing NoProxy 0 
        (TextureSize2D (fromIntegral $ width tex) (fromIntegral $ height tex)) 0 c
  return texName

{-calcMipMaps :: CompressedTexture -> [(Int, Int)] -- offset, size-}
{-calcMipMaps -}
