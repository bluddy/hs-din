module Texture (
    Texture(..)
    ) where

import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word


data Texture = Texture {width::Int, 
                        height::Int, 
                        unknownX::Int, 
                        unknownY::Int, 
                        offset::Int,
                        size::Int,
                        ptr::ForeignPtr Word8}

instance Show Texture where
    show t = "width: " ++ show (width t) ++
            " height: " ++ show (height t) ++
            " offset: " ++ show (offset t) ++
            " size: " ++ show (size t)

