import           Bresenham       (bresenham)

import           Data.Either     (fromRight)
import           Data.List       (sort, unfoldr, foldl')
import           Data.Vector     (Vector, (//))
import           HGraphics.Image (createImage, PixelRGB8(..), CreateImageInfo(..), SaveTGAInfo(..), saveTga)
import           HGraphics.Obj   (loadObjToWireframe)
import           HGraphics.Type  (Vec2(..), Vec4(..), Wireframe)
import Control.Monad (forM_)

basePath = "/Users/liuzichao/HGraphics/app/tinyrenderer"
outputPath = basePath ++ "/output/a.tga"
africanHeadPath = basePath ++ "/res/african_head.obj"

imageSize :: Vec2 Int
imageSize = Vec2 100 100

main :: IO ()
main = do
  print "a"
  -- let canvas = createImage CreateImageInfo{size = imageSize, pixel = PixelRGB8 0 0 0}
  -- wireframeLS <- loadObjToWireframe africanHeadPath
  -- let imageData = foldl' (\d (a, b) ->
  --                           let aWS = floor (a * imageSize)
  --                               bWS = floor (b * imageSize)
  --                               ps = bresenham (aWS.x, aWS.y) (bWS.x, bWS.y)
  --                               mapFunc p = (p.x + p.y * imageSize.width, PixelRGB8 1 1 1)
  --                           in  d // map mapFunc ps) canvas.imageData wireframeLS
  -- saveTga SaveTGAInfo{filepath = outputPath, image = canvas}
