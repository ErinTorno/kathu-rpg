{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Kathu.World.WorldSpace where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Kathu.Entity.Item
import Kathu.Graphics.Color (black)
import Kathu.Graphics.Drawable
import Kathu.Graphics.Palette
import Kathu.World.Field
import Linear.V3 (V3(..))

data WorldSpace = WorldSpace
    { worldID :: Text
    , worldPalettes :: Vector Palette
    , loadPoint   :: V3 Float
    , worldItems  :: Vector (V3 Float, ItemStack)
    , worldFields :: FieldSet
    -- Some way to hold the fields that it possesses
    }

emptyWorldSpace :: WorldSpace
emptyWorldSpace = WorldSpace "" (Vec.singleton (Palette black id)) (V3 0 0 0) Vec.empty Map.empty

-- right now we only get the one in range; later we will get surrounding so that even on borders everything will be drawn
fieldsSurrounding :: RealFrac a => V3 a -> WorldSpace -> [(V3 Int, Field)]
fieldsSurrounding v ws = catMaybes [readField v]
    where fields    = worldFields ws
          readField v' = let fv = fieldContainingCoord v' in ((fv,) <$> Map.lookup fv fields)