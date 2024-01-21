module HGraphics.Obj (
  loadObjToWireframe
) where

import           Codec.Wavefront
import           Codec.Wavefront.IO (fromFile)
import           Data.List          (foldl')
import           Data.Vector        (empty, (!))
import           Debug.Trace        (trace)
import           GHC.IO             (FilePath)
import           HGraphics.Type     (Vec4 (..), Wireframe)

loadObjToWireframe :: FilePath -> IO Wireframe
loadObjToWireframe fp = objToWireframe <$> loadObj fp

loadObj :: FilePath -> IO WavefrontOBJ
loadObj fp = fromFile fp >>= either error return

objToWireframe :: WavefrontOBJ -> Wireframe
-- objToWireframe obj = over (traverse ^..) obj.objFaces
objToWireframe obj = error ""
-- objToWireframe obj = foldl' func [] obj.objFaces
--   where func acc elementFace =
--           let (Face faceIndex1 faceIndex2 faceIndex3 _) = elementFace.elValue
--               locations = obj.objLocations
--               location1 = locations ! (faceIndex1.faceLocIndex - 1)
--               location2 = locations ! (faceIndex2.faceLocIndex - 1)
--               location3 = locations ! (faceIndex3.faceLocIndex - 1)
--               v1 = Vec4 location1.locX location1.locY location1.locZ location1.locW
--               v2 = Vec4 location2.locX location2.locY location2.locZ location2.locW
--               v3 = Vec4 location3.locX location3.locY location3.locZ location3.locW
--           in  (v2, v3) : (v1, v2) : (v1, v3) : acc
