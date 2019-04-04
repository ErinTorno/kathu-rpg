module Kathu.Network.Room where

import Data.Text (Text)

-- Random seeds are changed and sent everytime a player joins, to keep all on same page

data Room = Room
    { roomName :: Text
    , password :: Text
    , maxPlayers :: Int
    -- , host
    -- , otherPlayers
    }