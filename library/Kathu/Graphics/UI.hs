{-# LANGUAGE UndecidableInstances #-}

module Kathu.Graphics.UI where

import           Apecs
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types                (typeMismatch)
import           Data.Bifunctor                  as Bi
import           Data.Fixed                      (divMod')
import           Data.Functor.Compose            (getCompose)
import           Linear.V2                       (V2(..))
import           Verda.Graphics.Components
import           Verda.Graphics.Drawing          (pixelsPerUnit)
import           Verda.Graphics.Icons            (Icon)
import           Verda.Graphics.Sprites
import           Verda.Parsing.Aeson
import           Verda.Util.Apecs
import           Verda.Util.Dependency
import           Verda.World                     (VerdaWorld, addSpriteRenderExtension)

import           Kathu.Entity.ActorState
import           Kathu.Entity.Components         (Local)
import           Kathu.Entity.Resource

data DisplayBar = DisplayBar
    { startsAt        :: V2 Double
    -- for every N points that this bar measures, there will be a new unit of the image
    , pointsPerUnit   :: Double
    -- if present this will be drawn at the start and end of the bar
    , barCapBeginning :: Maybe Sprite
    , barCapEnding    :: Maybe Sprite
    , capWidth        :: Double
    , primaryWidth    :: Double
    , secondaryWidth  :: Double
    , primaryFull     :: Sprite
    , primary3Q       :: Sprite
    , primaryHalf     :: Sprite
    , primary1Q       :: Sprite
    , secondaryFull   :: Sprite
    , secondaryEmpty  :: Sprite
    }

data UIConfig = UIConfig
    { isEnabled :: Bool
    , gameIcon  :: Icon
    , healthBar :: DisplayBar
    , manaBar   :: DisplayBar
    }

instance (FromJSON (Dependency s m Sprite), Monad m) => FromJSON (Dependency s m DisplayBar) where
    parseJSON (Object v) = getCompose $ DisplayBar
        <$> fmap pos (v .:^ "starts-at")
        <*> v .:^ "points-per-part"
        <*> maybeSpr "cap-beginning"
        <*> maybeSpr "cap-ending"
        <*> pos (v .:^? "cap-width" .!=- 0)
        <*> pos (v .:^ "primary-width") <*> pos (v .:^ "secondary-width")
        <*> spr "primary-full" <*> spr "primary-3q" <*> spr "primary-half" <*> spr "primary-1q"
        <*> spr "secondary-full" <*> spr "secondary-empty"
        where incrLayer    = over spriteLayer (+1000) -- ensure floating above all other screen elems
              spr      idt = incrLayer <$> (v .:- idt)
              maybeSpr idt = fmap incrLayer <$> (v .:-? idt)
              pos :: Functor f => f Double -> f Double
              pos = fmap (/pixelsPerUnit) -- configured with pixels, converted to units here
    parseJSON v = typeMismatch "DisplayBar" v

instance ( FromJSON (Dependency s m Icon)
         , FromJSON (Dependency s m Sprite)
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

-- TODO convert to plain old UI when that gets added to Verda library
addUIExtension :: (ReadWriteEach w IO '[ActorState, Local, UIConfig], VerdaWorld w IO) => SystemT w IO ()
addUIExtension =
    addSpriteRenderExtension $ \_ renderSprite camPos screenDim idx -> do
        config  <- get global
        localAS <- getUnique
        let topLeft = camPos - ((*0.5) <$> screenDim)
        case localAS of
            Nothing -> pure idx
            Just (as, _ :: Local) -> liftIO $ drawBar renderSprite topLeft (as^.health) (healthBar config) idx
                                 >>= liftIO . drawBar renderSprite topLeft (as^.mana) (manaBar config)

drawBar :: RenderSpriteFn -> V2 Double -> Dynamic Double -> DisplayBar -> Int -> IO Int
drawBar renderSprite (V2 absX absY) dyn (DisplayBar (V2 sx sy) ppu capBegin capEnd capw pw sw pi4 pi3 pi2 pi1 sif sie) initIdx = snd <$> go
    where go = drawCap capBegin (absX + sx, initIdx) >>= goInner >>= drawCap capEnd
          goInner xIdxPair = drawN fullUnits sif sw xIdxPair >>= drawAt primarySpr pw >>= drawN emptyUnits sie sw
          drawCap cap xIdxPair = maybe (pure xIdxPair) (\spr -> drawAt spr capw xIdxPair) cap
          unitCount :: Int
          unitCount  = ceiling $ totalMaximum dyn / ppu
          fullUnits, partialRem :: Int
          (fullUnits, partialRem) = fixPartial . Bi.second (ceiling . (*4) . (/ppu)) $ (dyn^.dynCur) `divMod'` ppu
          -- if we have perfectly rounded, we drop the full units by one to draw a large one of four quarters
          fixPartial (n, 0) = (n - 1, 4)
          fixPartial p = p
          emptyUnits = unitCount - fullUnits - 1
          primarySpr = case partialRem of {1 -> pi1; 2 -> pi2; 3 -> pi3; 4 -> pi4; _ -> pi4}
          drawN 0 _ _   xIdxPair = pure xIdxPair
          drawN i spr w xIdxPair = drawAt spr w xIdxPair >>= drawN (i - 1) spr w
          drawAt  spr w (x, idx) = renderSprite idx spr (V2 (x + 0.5 * w) (sy + absY)) noTint >>= \idx' -> pure (x + w, idx')