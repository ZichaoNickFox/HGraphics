module HGraphics.Vec.Vec2 where

import Control.Lens
import Data.Default

data Vec2 a = Vec2 !a !a deriving (Show, Eq)

class R1 t where
  _x :: Lens' (t a) a
class R1 t => R2 t where
  _y :: Lens' (t a) a
  
instance Functor Vec2 where
  fmap f (Vec2 a b) = Vec2 (f a) (f b)
  {-# INLINE fmap #-}

instance Applicative Vec2 where
  pure a = Vec2 a a
  {-# INLINE pure #-}
  Vec2 fc fd <*> Vec2 c d = Vec2 (fc c) (fd d)
  {-# INLINE (<*>) #-}

instance Num a => Num (Vec2 a) where
  (+) = liftA2 (+)

instance Default (Vec2 Int) where
  def = Vec2 def def
instance Default (Vec2 Float) where
  def = Vec2 def def