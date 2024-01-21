module HGraphics.Image (
  createImage,
  saveTga,
  CP.PixelRGB8(..),
  CP.PixelRGBA8(..),
  CreateImageInfo(..),
  SaveTGAInfo(..)
) where

-- https://hackage.haskell.org/package/JuicyPixels-3.3.8
import qualified Codec.Picture     as CP (Image, Pixel (..), PixelRGB8 (..),
                                          PixelRGBA8 (..), generateImage,
                                          writeTga)
import qualified Codec.Picture.Tga as CPT (TgaSaveable)
import           Data.Default      (Default, def)
import           HGraphics.Type    (Vec2 (..))

type Image a = CP.Image a

data ImageFormat = TGA deriving (Show, Eq)

data (CP.Pixel px) => CreateImageInfo px
  = CreateImageInfo { size :: Vec2 Int, pixel :: px }

createImage :: (CP.Pixel px) => CreateImageInfo px -> Image px
-- FIXME
createImage info = CP.generateImage (\_ _ -> info.pixel) 1 1

data (CPT.TgaSaveable px) => SaveTGAInfo px
  = SaveTGAInfo { filepath :: FilePath, image :: Image px }

saveTga :: (CPT.TgaSaveable px) => SaveTGAInfo px -> IO ()
saveTga info = CP.writeTga info.filepath info.image

instance Default CP.PixelRGB8 where
  def = CP.PixelRGB8 def def def
instance Default (CreateImageInfo CP.PixelRGB8) where
  def = CreateImageInfo{size=def, pixel=def}
