module Mesh where

import Global
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Loader as L
import Data.Functor ((<$>))

data Mesh = Mesh {
            surfaces::[Surface],
            meshNumV::Int,
            vertexOffsets::StorableArray GLfloat,
            vertexBones::UArray (Int, Double) -- bone index, weight
           }

data Surface = Surface {
            surfaceNum::Int,
            surfNumV::Int,
            surfNumT::Int,
            tris::StorableArray GLint, -- triangles
            normals::StorableArray GLfloat,
            textureCoords::StorableArray GLfloat,
            collapseMap::[Int]
           }

offsetError offset cur = error $ "Offset " ++ show offset 
    ++ " not equal to " ++ show cur

parseMesh :: BLC.ByteString -> Mesh
parseMesh s = runGet get s
  where get = do
        magic <- getWord32le
        if magic /= 0x12121212 then error $ "Bad magic1 " ++ show magic
        else do
          version <- getWord32le
          numSurfaces <- getWord32le
          numTris <- getWord32le
          numVerts <- getWord32le
          surfaceOffset <- getWord32le
          trisOffset <- getWord32le
          vertsOffset <- getWord32le
          weightsOffset <- getWord32le
          collapseOffset <-
            if bytesRead == surfaceOffset
            then return Nothing
            else do 
              c <- getWord32le
              return Just c
          if bytesRead /= surfaceOffset 
          then offsetError surfaceOffset bytesRead 
          else do
            surfaces <- mapM (parseSurface collapseOffset) [1..numSurfaces]
            if bytesRead /= trisOffset
            then offsetError trisOffset bytesRead
            else do
              tris <- mapM parseTris surfaces
              if bytesRead /= vertsOffset
              then offsetError vertsOffset bytesRead
              else do
                verts <- mapM parseVertices surfaces
                let numWeights = foldl (\acc (_,_,_,_,_,i,_) -> acc + i) 0 verts
                if bytesRead /= weightsOffset
                then offsetError weightsOffset bytesRead
                else do
                  weights <- mapM parseWeight [1..numWeights]
                  collapse <- 
                    case collapseOffset of
                        Just x | x /= bytesRead -> offsetError x bytesRead
                               | x == bytesRead -> 
                                   mapM parseCollapses surface
                        Nothing -> Nothing
                return $ createMesh surfaces tris verts weights collapse
                  
parseSurface c _ = do
    surfaceNum <- getWord32le
    numVerts <- getWord32le
    numTris <- getWord32le
    vertsOffset <- getWord32le
    trisOffset <- getWord32le
    collapseOffset <- case c of
        Nothing -> Nothing
        _       -> Just <$> getWord32le
    return 
       (surfaceNum, numVerts, numTris, vertsOffset, trisOffset, collapseOffset)

parseTris (_,_,n,_,_,_) = mapM parseTris [1..n]

parseTri :: a -> Get (Int,Int,Int)
parseTri _ = do
    x <- getWord32le
    y <- getWord32le
    z <- getWord32le
    return (x,y,z)

parseVertices (_,n,_,_,_,_) = mapM parseVertex [1..n]

parseVertex = a -> Get (Float, Float, Float, Float, Float, Int, Int)
parseVertex _ = do
    u <- getWord32le
    v <- getWord32le
    x <- getWord32le
    y <- getWord32le
    z <- getWord32le
    numBones <- getWord32le
    firstBone <- getWord32le
    return (u,v,x,y,z,numBones,firstBone)

parseWeight :: a -> (Int, Float, Float, Float, Float)
parseWeight _ = do
    ix <- getWord32le
    x <- getWord32le
    y <- getWord32le
    z <- getWord32le
    weight <- getWord32le
    return (ix,x,y,z,weight)

parseCollapses (_,n,_,_,_,_) = mapM parseCollapse [1..n]
parseCollapse _ = getWord32le

createMesh ss ts vs ws cs =
    

createSurfaces (s:ss) (t:ts) (v:vs) (c:cs) = 
    (createSurface s t v c):createMesh ss ts vs cs

createSurface s@(num, numV, numT, _, _, _) ts vs cs =
  Surface {
    surfaceNum=num,
    surfNumV=numV,
    surfNumT=numT,
    tris=listArray $ concat $ map (\(x,y,z) -> x:y:z:[]) ts,
    normals=listArray $ concat $ map (\(_,_,x,y,z,_,_) -> x:y:z:[]) vs,
    textureCoords=listArray $ concat $ map (\(u,v,_,_,_,_,_) -> u:v:[]) vs,
    collapseMap=cs
   }


