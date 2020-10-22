{-# LANGUAGE UndecidableInstances #-}

module Kathu.App.Graphics.UI where

import           Apecs
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Bifunctor              as Bi
import           Data.Fixed (divMod')
import           Data.Functor.Compose (getCompose)
import           Linear.V2 (V2(..))
import qualified SDL
import           Verda.Graphics.Icons            (Icon)
import           Verda.Graphics.SpriteManager    (SpriteManager)
import           Verda.Graphics.Sprites          (SpriteID)

import           Kathu.App.Graphics.Drawing
import           Kathu.Entity.ActorState
import           Kathu.Entity.Resource
import           Kathu.Graphics.Drawable
import           Verda.Parsing.Aeson
import           Verda.Util.Dependency

data DisplayBar = DisplayBar
    { startsAt        :: V2 Double
    -- for every N points that this bar measures, there will be a new unit of the image
    , pointsPerUnit   :: Double
    -- if present this will be drawn at the start and end of the bar
    , barCapBeginning :: Maybe (RenderSprite SpriteID)
    , barCapEnding    :: Maybe (RenderSprite SpriteID)
    , capWidth        :: Double
    , primaryWidth    :: Double
    , secondaryWidth  :: Double
    , primaryFull     :: RenderSprite SpriteID
    , primary3Q       :: RenderSprite SpriteID
    , primaryHalf     :: RenderSprite SpriteID
    , primary1Q       :: RenderSprite SpriteID
    , secondaryFull   :: RenderSprite SpriteID
    , secondaryEmpty  :: RenderSprite SpriteID
    }

data UIConfig = UIConfig
    { isEnabled :: Bool
    , gameIcon  :: Icon
    , healthBar :: DisplayBar
    , manaBar   :: DisplayBar
    }

instance (FromJSON (Dependency s m (RenderSprite SpriteID)), Monad m) => FromJSON (Dependency s m DisplayBar) where
    parseJSON (Object v) = getCompose $ DisplayBar
        <$> v .:^ "starts-at"
        <*> v .:^ "points-per-part"
        <*> v .:-? "cap-beginning"
        <*> v .:-? "cap-ending"
        <*> (v .:^? "cap-width" .!=- 0)
        <*> v .:^ "primary-width" <*> v .:^ "secondary-width"
        <*> v .:- "primary-full" <*> v .:- "primary-3q" <*> v .:- "primary-half" <*> v .:- "primary-1q"
        <*> v .:- "secondary-full" <*> v .:- "secondary-empty"
    parseJSON v = typeMismatch "DisplayBar" v

instance ( FromJSON (Dependency s m (RenderSprite SpriteID))
         , FromJSON (Dependency s m Icon)
         , FromJSON (Dependency s m SpriteID)
         , Monad m
         ) => FromJSON (Dependency s m UIConfig) where
    parseJSON (Object v) = getCompose $ UIConfig True
        <$> v .:- "game-icon"
        <*> v .:- "health-bar"
        <*> v .:- "mana-bar"
    parseJSON v = typeMismatch "UIConfig" v

---------------
-- Functions --
---------------

barIconBleed :: Floating a => a
barIconBleed = 1.01

renderUI :: forall w m. (MonadIO m, Get w m UIConfig, Get w m SpriteManager) => SDL.Renderer -> Double -> Maybe ActorState -> SystemT w m ()
renderUI _ _ Nothing = pure ()
renderUI renderer scale (Just as) = do
    config <- get global
    manager <- get global
    
    drawBar renderer scale manager (as^.health) (healthBar config)
    drawBar renderer scale manager (as^.mana)   (manaBar config)
    pure ()

drawBar :: MonadIO m => SDL.Renderer -> Double -> SpriteManager -> Dynamic Double -> DisplayBar -> m ()
drawBar renderer scale mgr dyn (DisplayBar startingPos ppu capBegin capEnd capw pw sw pi4 pi3 pi2 pi1 sif sie) = go >> pure ()
    where go = drawCap capBegin sx >>= goInner >>= drawCap capEnd
          goInner x = drawN fullUnits sif sw x >>= drawAt primaryImg pw >>= drawN emptyUnits sie sw
          drawCap cap x = maybe (pure x) (\i -> drawAt i capw x) cap
          (V2 sx sy) = (*scale) <$> startingPos
          unitCount :: Int
          unitCount  = ceiling $ totalMaximum dyn / ppu
          fullUnits, partialRem :: Int
          (fullUnits, partialRem) = fixPartial . Bi.second (ceiling . (*4) . (/ppu)) $ (dyn ^. dynCur) `divMod'` ppu
          -- if we have perfectly rounded, we drop the full units by one to draw a large one of four quarters
          fixPartial (n, 0) = (n - 1, 4)
          fixPartial p = p
          emptyUnits = unitCount - fullUnits - 1
          primaryImg = case partialRem of {1 -> pi1; 2 -> pi2; 3 -> pi3; 4 -> pi4; _ -> pi4}
          drawAt img w x = blitRenderSprite renderer mgr (mkRenderRectNoCenter barIconBleed scale (V2 x sy)) img >> pure (x + scale * w)
          drawN 0 _ _ x   = pure x
          drawN i img w x = drawAt img w x >>= drawN (i - 1) img w