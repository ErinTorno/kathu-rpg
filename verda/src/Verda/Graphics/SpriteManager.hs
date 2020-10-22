module Verda.Graphics.SpriteManager
    ( SpriteManager
    , mkSpriteManager
    , fetchTexture
    -- no-op to remove
    , nextPaletteManager
    , setPaletteIdx
    , setPaletteManager
    , currentPalette
    , availablePaletteCount
    , loadPalettes
    ) where

import           Apecs                  hiding (($=))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vec
import qualified SDL
import           SDL                    (($=))

import           Verda.Graphics.Sprites
import           Verda.Util.Types       (Identifier, IDMap)

newtype SpriteManager = SpriteManager (Vector SDL.Texture)

instance Semigroup SpriteManager where (<>) = mappend
instance Monoid SpriteManager where mempty = error "Attempted to use SpriteManager before it has been initialized"
instance Component SpriteManager where type Storage SpriteManager = Global SpriteManager

mkSpriteManager :: MonadIO m => SDL.Renderer -> SurfaceVector -> m SpriteManager
mkSpriteManager renderer surfaces = do
    defScaling <- SDL.get SDL.HintRenderScaleQuality

    let mkTexture (surface, Nothing) =
           SDL.createTextureFromSurface renderer surface
        mkTexture (surface, Just scaling) = do
           SDL.HintRenderScaleQuality $= scaling
           texture <- SDL.createTextureFromSurface renderer surface
           SDL.HintRenderScaleQuality $= defScaling
           pure texture
        
    sprites <- Vec.mapM mkTexture surfaces
    SDL.HintRenderScaleQuality $= defScaling
    pure $ SpriteManager sprites

fetchTexture :: SpriteManager -> SpriteID -> SDL.Texture
fetchTexture (SpriteManager v) (SpriteID idx) = v Vec.! idx

-- No-op implementations to help with porting; remove later

nextPaletteManager :: SpriteManager -> Identifier
nextPaletteManager _ = ""

setPaletteIdx :: Monad m => Int -> SystemT w m ()
setPaletteIdx _ = pure ()

setPaletteManager :: Monad m => Identifier -> m Bool
setPaletteManager _ = pure True

currentPalette :: SpriteManager -> Int
currentPalette _ = 0

availablePaletteCount :: SpriteManager -> Int
availablePaletteCount _ = 1

loadPalettes :: Monad m => IDMap a -> SpriteManager -> m SpriteManager
loadPalettes _ = pure