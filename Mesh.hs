module Mesh where

import Global
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Loader as L
import Data.Functor ((<$>))
import qualified Data.List as List
import qualified CastFloat as CF

type Offset = (Float, Float, Float)
type Bone = (Int, Double)

data Mesh = Mesh {
            surfaces::[Surface],
            vertexOffsets::[Offset],
            vertexBones::[Bone] -- bone index, weight
           } deriving Show

type Tri = (Int, Int, Int)
type Normal = (Float, Float, Float)
type UV = (Float, Float)
type VertData = (Int, Int) -- Numbones, Firstbone

data Surface = Surface {
            surfaceNum::Int,
            surfNumV::Int,
            surfNumT::Int,
            tris::[Tri],
            normals::[Normal],
            textureCoords::[UV],
            vertData::[VertData]
           } deriving Show

offsetError offset cur = error $ "Offset " ++ show offset 
    ++ " not equal to " ++ show cur

parseMesh :: BLC.ByteString -> Mesh
parseMesh s = runGet get s
  where get = do
        magic <- getWord32le
        if magic /= 0x12121212 then error $ "Bad magic1 " ++ show magic
        else do
          version <- fromIntegral <$> getWord32le
          numSurfaces <- fromIntegral <$> getWord32le
          numTris <- fromIntegral <$> getWord32le
          numVerts <- fromIntegral <$> getWord32le
          surfaceOffset <- fromIntegral <$> getWord32le
          trisOffset <- fromIntegral <$> getWord32le
          vertsOffset <- fromIntegral <$> getWord32le
          weightsOffset <- fromIntegral <$> getWord32le
          b <- bytesRead
          collapseOffset <-
            if b == fromIntegral surfaceOffset
            then return Nothing
            else Just <$> getWord32le
          b <- bytesRead
          if b /= fromIntegral surfaceOffset 
          then offsetError surfaceOffset b 
          else do
            surfaces <- mapM (parseSurface collapseOffset) [1..numSurfaces]
            b <- bytesRead
            if b /= fromIntegral trisOffset
            then offsetError trisOffset b
            else do
              tris <- mapM parseTris surfaces
              b <- bytesRead
              if b /= fromIntegral vertsOffset
              then offsetError vertsOffset b
              else do
                verts <- mapM parseVertices surfaces
                let numWeights = foldl (\acc v -> 
                                    foldl (\acc' (_,_,i,_) -> acc' + i) acc v)
                                    0 verts
                b <- bytesRead
                if b /= fromIntegral weightsOffset
                then offsetError weightsOffset b
                else do
                  weights <- mapM parseWeight [1..numWeights]
                  b <- bytesRead
                  collapse <- 
                    case collapseOffset of
                        Just x | fromIntegral x /= b -> offsetError x b
                               | otherwise -> 
                                   Just <$> mapM parseCollapses surfaces
                        Nothing -> return Nothing
                  return $ createMesh surfaces tris verts weights collapse
                  
parseSurface :: Maybe a -> a -> Get (Int, Int, Int, Int, Int, Maybe Int)
parseSurface c _ = do
    surfaceNum <- fromIntegral <$> getWord32le
    numVerts <- fromIntegral <$> getWord32le
    numTris <- fromIntegral <$> getWord32le
    vertsOffset <- fromIntegral <$> getWord32le
    trisOffset <- fromIntegral <$> getWord32le
    collapseOffset <- case c of
        Nothing -> return Nothing
        _       -> Just . fromIntegral <$> getWord32le
    return 
       (surfaceNum, numVerts, numTris, vertsOffset, trisOffset, collapseOffset)

parseTris (_,_,n,_,_,_) = mapM parseTri [1..n]

parseTri :: a -> Get Tri
parseTri _ = do
    x <- fromIntegral <$> getWord32le
    y <- fromIntegral <$> getWord32le
    z <- fromIntegral <$> getWord32le
    return (x, y, z)

parseVertices (_,n,_,_,_,_) = mapM parseVertex [1..n]

parseVertex :: a -> Get (UV, Normal, Int, Int)
parseVertex _ = do
    u <- CF.wordToFloat <$> getWord32le
    v <- CF.wordToFloat <$> getWord32le
    x <- CF.wordToFloat <$> getWord32le
    y <- CF.wordToFloat <$> getWord32le
    z <- CF.wordToFloat <$> getWord32le
    numBones <- fromIntegral <$> getWord32le
    firstBone <- fromIntegral <$> getWord32le
    return ((u,v),(x,y,z),numBones,firstBone)

parseWeight :: a -> Get (Bone, Offset)
parseWeight _ = do
    ix <- fromIntegral <$> getWord32le
    x <- CF.wordToFloat <$> getWord32le
    y <- CF.wordToFloat <$> getWord32le
    z <- CF.wordToFloat <$> getWord32le
    weight <- CF.wordToFloat <$> getWord32le
    let weight' = realToFrac weight
    return ((ix,weight'),(x,y,z))

parseCollapses (_,n,_,_,_,_) = mapM parseCollapse [1..n]
parseCollapse _ = fromIntegral <$> getWord32le

createMesh ss ts vs ws cs =
    let surfaces = createSurfaces ss ts vs 
        (bones, offsets) = List.unzip ws
    in Mesh surfaces offsets bones

createSurfaces (s:ss) (t:ts) (v:vs) = 
    (createSurface s t v):createSurfaces ss ts vs
createSurfaces [] _ _ = []

createSurface s@(num, numV, numT, _, _, _) ts vs =
  let ns = map (\(_,n,_,_) -> n) vs
      uvs = map (\(u,_,_,_) -> u) vs
      vertD = map (\(_,_,i,first) -> (i,first)) vs
  in Surface num numV numT ts ns uvs vertD

parseFile :: FileName -> IO Mesh
parseFile path = do
    fileMap <- L.fullFileMap
    fileStr <- L.readPath fileMap path
    return $ parseMesh fileStr

showFile :: Mesh -> IO String
showFile mesh = return $ show mesh
