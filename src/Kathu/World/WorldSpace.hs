{-# LANGUAGE BangPatterns #-}
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
import Kathu.Entity.Prototype
import Kathu.Graphics.Color (black)
import Kathu.Graphics.Drawable
import Kathu.Graphics.Palette
import Kathu.World.Field
import Linear.V3 (V3(..))

data WorldSpace = WorldSpace
    { worldID :: Text
    , worldName :: Text
    , worldPalettes :: Vector Palette
    , loadPoint   :: V3 Float
    , worldEntities :: Vector (V3 Float, EntityPrototype)
    , worldItems  :: Vector (V3 Float, ItemStack)
    , worldFields :: FieldSet
    -- Some way to hold the fields that it possesses
    }

emptyWorldSpace :: WorldSpace
emptyWorldSpace = WorldSpace "" "the void" (Vec.singleton (Palette black Nothing)) (V3 0 0 0) Vec.empty Vec.empty Map.empty

-- right now we only consider horizontal fields; ones with different z depths are ignored
fieldsSurrounding :: RealFrac a => V3 a -> WorldSpace -> [(V3 Int, Field)]
fieldsSurrounding v ws = catMaybes $ readFields [] (ox - 1) (oy - 1)
    where fields    = worldFields ws
          (V3 ox oy oz) = fieldContainingCoord v
          readFields !acc !x !y | y > oy + 1 = acc
                                | x > ox + 1 = readFields acc 0 (y + 1)
                                | otherwise  = readFields (((curV,) <$> Map.lookup curV fields):acc) (x + 1) y
              where curV = V3 x y oz -- only consider same z level right now