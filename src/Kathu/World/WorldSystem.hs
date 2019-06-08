module Kathu.World.WorldSystem where

import Apecs
import qualified Data.Vector as Vec
import Kathu.Entity.Components
import Kathu.Entity.Item
import Kathu.Entity.Prototype
import Kathu.Entity.System
import Kathu.Graphics.ImageManager
import Kathu.World.Field
import Kathu.World.WorldSpace
import Linear.V3 (V3(..))

initWorldSpace :: WorldSpace -> SystemT' IO ()
initWorldSpace ws = do
    global $= ws
    prevManager <- get global
    manager <- loadPalettes (worldPalettes ws) prevManager
    global $= manager

    cmap $ \(Local _, Position _) -> Position . loadPoint $ ws
    -- we place all items in the world as entities
    mapM_ (\(pos, ety) -> newFromPrototype ety >>= (flip set) (Position pos)) (worldEntities ws)
    mapM_ (\(pos, item) -> newEtyFromItem item pos) (worldItems ws)

newEtyFromItem :: ItemStack -> V3 Float -> SystemT' IO Entity
newEtyFromItem stack v = newEntity (Position v, render)
    where render = Render . Vec.singleton . itemIcon . stackItem $ stack
          -- as of right now, count not considered; this will be added when picking up is implemented