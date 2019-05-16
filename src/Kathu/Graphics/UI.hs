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
import Kathu.Graphics.ImageManager
import Kathu.Util.Misc (mapSnd)
import Linear.V2 (V2(..))
import qualified Kathu.Util.SDLCommon as SDLC

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

{-
drawBar :: (MonadIO m, Num a) => Float -> Dynamic a -> DisplayBar -> m ()
drawBar scale dyn (DisplayBar (V2 sx sy) ppu bcap capw pw sw pi4 pi3 pi2 pi1 sif sie) = go
    where go = maybe (pure ()) (drawN 1 0 sx) bcap >> replicateM_ fullUnits >> 
          unitCount  = ceiling $ (totalMaximum dyn) / ppu
          (fullUnits, partialRem) = mapSnd (/ppu) $ (dyn ^. dynCur) `divMod'` ppu
          emptyUnits = unitCount - fullUnits - 1
          drawN 0 _ _ _   = pure ()
          drawN i w x img = drawN (i - 1) w (x + w) >>
-}

renderUI :: forall w m. (Get w m UIConfig, Get w m ImageManager) => Float -> SystemT w m ()
renderUI scale = do
    --config <- get global
    pure ()