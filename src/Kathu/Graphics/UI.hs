{-# LANGUAGE ExplicitForAll, FlexibleContexts #-}

module Kathu.Graphics.UI where

import Apecs
import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Fixed (divMod')
import Kathu.Entity.ActorState
import Kathu.Entity.Resource
import Kathu.Graphics.Drawable
import Kathu.Graphics.Drawing
import Kathu.Graphics.ImageManager
import Kathu.Util.Misc (mapSnd)
import Linear.V2 (V2(..))
import qualified Kathu.Util.SDLCommon as SDLC
import qualified SDL

data DisplayBar = DisplayBar
    { startsAt       :: V2 Float
    -- for every N points that this bar measures, there will be a new unit of the image
    , pointsPerUnit  :: Float
    -- if present this will be drawn at the start and end of the bar
    , barCap         :: Maybe RenderSprite
    , capWidth       :: Float
    , primaryWidth   :: Float
    , secondaryWidth :: Float
    , primaryFull    :: RenderSprite
    , primary3Q      :: RenderSprite
    , primaryHalf    :: RenderSprite
    , primary1Q      :: RenderSprite
    , secondaryFull  :: RenderSprite
    , secondaryEmpty :: RenderSprite
    }

data UIConfig = UIConfig
    { isEnabled :: Bool
    , gameIcon  :: ImageID
    , healthBar :: DisplayBar
    , manaBar   :: DisplayBar
    }

barIconBleed :: Float
barIconBleed = 1.01

renderUI :: forall w m. (MonadIO m, Get w m UIConfig, Get w m ImageManager) => SDL.Surface -> Float -> Maybe ActorState -> SystemT w m ()
renderUI _ _ Nothing = pure ()
renderUI screen scale (Just as) = do
    config <- get global
    manager <- get global
    
    drawBar scale manager (as ^. health) (healthBar config) screen
    drawBar scale manager (as ^. mana) (manaBar config) screen
    pure ()

drawBar :: MonadIO m => Float -> ImageManager -> Dynamic Float -> DisplayBar -> SDL.Surface -> m ()
drawBar scale im dyn (DisplayBar startsAt ppu bcap capw pw sw pi4 pi3 pi2 pi1 sif sie) sur = go >> pure ()
    where go = drawCap sx >>= goInner >>= drawCap
          goInner x = drawN fullUnits sif sw x >>= drawAt primaryImg pw >>= drawN emptyUnits sie sw
          drawCap x = maybe (pure x) (\i -> drawAt i capw x) bcap
          (V2 sx sy) = (*scale) <$> startsAt
          unitCount :: Int
          unitCount  = ceiling $ (totalMaximum dyn) / ppu
          fullUnits, partialRem :: Int
          (fullUnits, partialRem) = fixPartial . mapSnd (ceiling . (*4) . (/ppu)) $ (dyn ^. dynCur) `divMod'` ppu
          -- if we have perfectly rounded, we drop the full units by one to draw a large one of four quarters
          fixPartial (n, 0) = (n - 1, 4)
          fixPartial p = p
          emptyUnits = unitCount - fullUnits - 1
          primaryImg = case partialRem of {1 -> pi1; 2 -> pi2; 3 -> pi3; 4 -> pi4; _ -> pi4}
          drawAt img w x = blitRenderSprite im (mkRenderRectNoCenter barIconBleed (V2 0 0) scale (V2 x sy)) img sur >> pure (x + scale * w)
          drawN 0 _ _ x   = pure x
          drawN i img w x = drawAt img w x >>= drawN (i - 1) img w