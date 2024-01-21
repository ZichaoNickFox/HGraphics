module HGraphics.Vec.Vec4 where

import HGraphics.Vec.Vec3
import Control.Lens
import Data.Default

data Vec4 a = Vec4 !a !a !a !a deriving (Show, Eq)

class R3 t => R4 t where
  _w :: Lens' (t a) a

instance Functor Vec4 where
  fmap f (Vec4 a b c d) = Vec4 (f a) (f b) (f c) (f d)
  {-# INLINE fmap #-}

instance Applicative Vec4 where
  pure a = Vec4 a a a a
  {-# INLINE pure #-}
  Vec4 fe ff fg fh <*> Vec4 e f g h = Vec4 (fe e) (ff f) (fg g) (fh h)
  {-# INLINE (<*>) #-}

instance Num a => Num (Vec4 a) where
  (+) = liftA2 (+)

instance Default (Vec4 Int) where
  def = Vec4 def def def def
instance Default (Vec4 Float) where
  def = Vec4 def def def def