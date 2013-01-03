module Model where

import Global
import qualified Loader as L
import qualified Skeleton as Skel
import qualified Mesh as M
import qualified Skin as Skin
import qualified Animation as A
import qualified ModelLoader as ML
import qualified Data.ByteString.Lazy.Char8 as BLC

data Model = Model {
              skeleton::Skel.Skeleton,
              mesh::M.Mesh,
              skin::Skin.Skin,
              objectType::Maybe String,
              animations::[A.Animation]
              } deriving Show

loadModelFromFile :: FileName -> IO Model 
loadModelFromFile path = do
    fileMap <- L.fullFileMap
    fileStr <- L.readPath fileMap path
    let ml = ML.doParse $ BLC.unpack fileStr
        dir = ML.dir ml
    skelStr <- L.readPath fileMap $ dir++(ML.skel ml)
    let skel = Skel.parseSkel skelStr
    meshStr <- L.readPath fileMap $ dir++(ML.mesh ml)
    let mesh = M.parseMesh meshStr
    skinStr <- L.readPath fileMap $ dir++(ML.skin ml)
    let skin = Skin.doParse $ BLC.unpack skinStr
    return $ Model skel mesh skin (ML.objectType ml) []    
    
        


    
             
    
    


