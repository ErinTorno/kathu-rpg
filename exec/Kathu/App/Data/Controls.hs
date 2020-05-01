{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

module Kathu.App.Data.Controls where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson
import           Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as UMVec
import           Data.Word
import           GHC.Generics
import qualified SDL

import           Kathu.Parsing.Aeson         (standardProjectOptions)
import           Kathu.Util.Collection       (forMVec)

type InputState = Word8

pattern BtnUnpressed, BtnPressed, BtnHeld, BtnReleased :: Word8
pattern BtnUnpressed = 0
pattern BtnPressed   = 1
pattern BtnHeld      = 2
pattern BtnReleased  = 3

isPressedOrHeld :: InputState -> Bool
isPressedOrHeld st = st == BtnPressed || st == BtnHeld

newtype InputCode = InputCode {unInputCode :: Word32}

fromScanCode :: SDL.Scancode -> InputCode
fromScanCode (SDL.Scancode w) = InputCode w

instance ToJSON InputCode where
    toJSON (InputCode w) = toJSON w
instance FromJSON InputCode where
    parseJSON o = InputCode <$> parseJSON o

-- We assign mouse presses just as keys, but since they don't have Scancodes we map them to ints after the scancode ints end
fromMouseButton :: SDL.MouseButton -> InputCode
fromMouseButton btn = case btn of
    SDL.ButtonLeft    -> InputCode 300 
    SDL.ButtonMiddle  -> InputCode 301
    SDL.ButtonRight   -> InputCode 302
    SDL.ButtonX1      -> InputCode 303
    SDL.ButtonX2      -> InputCode 304
    SDL.ButtonExtra _ -> InputCode 0 -- UNKNOWN, don't follow these right now

-- We force our input buffer to allocate room for this many
maxInputCode :: Integral a => a
maxInputCode = 305

newtype ControlState = ControlState {unControlState :: IOVector InputState}

mkControlState :: MonadIO m => m ControlState
mkControlState = ControlState <$> liftIO (UMVec.replicate maxInputCode BtnUnpressed)

getInputState :: MonadIO m => ControlState -> InputCode -> m InputState
getInputState (ControlState st) (InputCode code)
    | code >= maxInputCode = pure BtnUnpressed
    | otherwise            = liftIO $ UMVec.unsafeRead st (fromIntegral code)

-- Should be run before collecting input for this frame
-- | Moves unreleased buttons from just-pressed to held; and released ones to not-pressed
nextInputStateFrame :: MonadIO m => ControlState -> m ()
nextInputStateFrame (ControlState st) = liftIO $ forMVec st update
    where update BtnPressed  = BtnHeld
          update BtnReleased = BtnUnpressed
          update e           = e

updateInputCode :: MonadIO m => ControlState -> InputCode -> SDL.InputMotion -> m ()
updateInputCode (ControlState st) (InputCode code) motion =
    liftIO $ UMVec.unsafeWrite st (fromIntegral code) (newState motion)
    where newState SDL.Pressed  = BtnPressed
          newState SDL.Released = BtnReleased

data Controls = Controls
    { inputMoveNorth         :: !InputCode
    , inputMoveEast          :: !InputCode
    , inputMoveSouth         :: !InputCode
    , inputMoveWest          :: !InputCode
    , inputFocus             :: !InputCode
    , inputToggleDebug       :: !InputCode
    , inputDebugZoomIn       :: !InputCode
    , inputDebugZoomOut      :: !InputCode
    , inputDebugNextPalette  :: !InputCode
    , inputDebugPrintPhysics :: !InputCode
    } deriving (Generic)

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
    , inputToggleDebug       = fromScanCode SDL.ScancodeF3
    , inputDebugZoomIn       = fromScanCode SDL.ScancodeKPMinus
    , inputDebugZoomOut      = fromScanCode SDL.ScancodeKPPlus
    , inputDebugNextPalette  = fromScanCode SDL.ScancodeF5
    , inputDebugPrintPhysics = fromScanCode SDL.ScancodeF7
    }