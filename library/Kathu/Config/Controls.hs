module Kathu.Config.Controls where

import           Data.Aeson
import           GHC.Generics
import qualified SDL
import           Verda.Event.Controls (InputCode, fromScanCode)
import           Verda.Parsing.Aeson  (standardProjectOptions)

data Controls = Controls
    { inputMoveNorth         :: !InputCode
    , inputMoveEast          :: !InputCode
    , inputMoveSouth         :: !InputCode
    , inputMoveWest          :: !InputCode
    , inputFocus             :: !InputCode
    , inputInteract          :: !InputCode
    , inputToggleDebug       :: !InputCode
    , inputDebugZoomIn       :: !InputCode
    , inputDebugZoomOut      :: !InputCode
    , inputDebugNextPalette  :: !InputCode
    , inputDebugPrintPhysics :: !InputCode
    } deriving Generic

instance ToJSON Controls where
    toJSON = genericToJSON standardProjectOptions
instance FromJSON Controls where
    parseJSON = genericParseJSON standardProjectOptions

defaultControls :: Controls
defaultControls = Controls
    { inputMoveNorth         = fromScanCode SDL.ScancodeW
    , inputMoveEast          = fromScanCode SDL.ScancodeD
    , inputMoveSouth         = fromScanCode SDL.ScancodeS
    , inputMoveWest          = fromScanCode SDL.ScancodeA
    , inputFocus             = fromScanCode SDL.ScancodeLShift
    , inputInteract          = fromScanCode SDL.ScancodeE
    , inputToggleDebug       = fromScanCode SDL.ScancodeF3
    , inputDebugZoomIn       = fromScanCode SDL.ScancodeKPMinus
    , inputDebugZoomOut      = fromScanCode SDL.ScancodeKPPlus
    , inputDebugNextPalette  = fromScanCode SDL.ScancodeF5
    , inputDebugPrintPhysics = fromScanCode SDL.ScancodeF7
    }