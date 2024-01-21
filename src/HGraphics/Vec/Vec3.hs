module HGraphics.Vec.Vec3 where

import HGraphics.Vec.Vec2
import Control.Lens
import Data.Default

data Vec3 a = Vec3 !a !a !a deriving (Show, Eq)

class R2 t => R3 t where
  _z :: Lens' (t a) a
  
instance Functor Vec3 where
  fmap f (Vec3 a b c) = Vec3 (f a) (f b) (f c)
  {-# INLINE fmap #-}

instance Applicative Vec3 where
  pure a = Vec3 a a a
  {-# INLINE pure #-}
  Vec3 fd fe ff <*> Vec3 d e f = Vec3 (fd d) (fe e) (ff f)
  {-# INLINE (<*>) #-}

instance Num a => Num (Vec3 a) where
  (+) = liftA2 (+)

instance Default (Vec3 Int) where
  def = Vec3 def def def
instance Default (Vec3 Float) where
  def = Vec3 def def def