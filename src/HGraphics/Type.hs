module HGraphics.Type (
  Vec2(..),
  Vec3(..),
  Vec4(..),
  Wireframe
) where

import           Data.Default (Default, def)
import           Data.Vector  (Vector)
import     HGraphics.Vec.Vec2
import HGraphics.Vec.Vec3
import HGraphics.Vec.Vec4

type Wireframe = [(Vec4 Float, Vec4 Float)]
