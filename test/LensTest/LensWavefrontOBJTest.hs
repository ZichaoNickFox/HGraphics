module LensTest.LensWavefrontOBJTest (lensTest) where

import Control.Lens
import Codec.Wavefront
import Codec.Wavefront.IO
import Data.Vector hiding (length, foldr, reverse, sum)
import Test.HUnit

objPath :: String
objPath = "/Users/liuzichao/HGraphics/app/tinyrenderer/res/african_head.obj"
obj :: IO WavefrontOBJ
obj = fromFile objPath >>= either error return

faceVector :: WavefrontOBJ -> Vector (Element Face)
faceVector obj = obj.objFaces

faceElements :: Vector (Element Face) -> [Element Face]
faceElements fv = fv ^.. folded

faces :: [Element Face] -> [Face]
faces efs = over mapped elValue efs

temp :: IO ()
temp = do
  print "a"
  -- o <- obj
  -- print $ (faces . faceElements . faceVector) o

wavefrontTestList :: Test
wavefrontTestList = TestList [

  ]

lensTest :: IO Counts
lensTest = runTestTT wavefrontTestList