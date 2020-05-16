{-# LANGUAGE AllowAmbiguousTypes #-}

module Kathu.Editor.Util.PropertyGrid where
    
import           Control.Lens               hiding (set)
import           Control.Monad              (void)
import           Data.GI.Base
import           Data.Int
import           Data.IORef
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vec
import qualified GI.Gtk                     as Gtk

import           Kathu.Editor.Types
import           Kathu.Util.Flow            ((>>=/))

type PropertyRowAdder a = Int32 -> Gtk.Grid -> IORef a -> IO (EditableProperty a)

data RowMutability = MutableRow | ReadOnlyRow

class IsEditableProperty a w r where
    mkRowWith :: RowMutability -> (w -> IO ()) -> Text -> Lens' r a -> PropertyRowAdder r

    mkRow :: Text -> Lens' r a -> PropertyRowAdder r
    mkRow = mkRowWith MutableRow (\(_ :: w) -> pure ())

    mkReadOnlyRow :: Text -> Lens' r a -> PropertyRowAdder r
    mkReadOnlyRow = mkRowWith ReadOnlyRow (\(_ :: w) -> pure ())

mkPropertyGrid :: Gtk.Grid -> IORef a -> Vector (PropertyRowAdder a) -> IO (Vector (EditableProperty a))
mkPropertyGrid grid ref =
    Vec.imapM (\row adder -> adder (fromIntegral row) grid ref)

mkPropertyRow :: Gtk.IsWidget w => Text -> (w -> IO () -> IO ignored) -> IO w -> (w -> a -> IO a) -> (w -> a -> IO ()) -> PropertyRowAdder a
mkPropertyRow name onSignalChange mkWidget onWidgetChange onRefChange row grid ref = do
    label  <- Gtk.labelNewWithMnemonic (Just name)
    widget <- mkWidget
    void . onSignalChange widget $ do
        value  <- readIORef ref
        value' <- onWidgetChange widget value
        writeIORef ref value'

    Gtk.gridAttach grid label 0 row 1 1
    Gtk.gridAttach grid widget 1 row 1 1
    pure $ onRefChange widget

mkPropertyRowReadOnly :: Gtk.IsWidget w => Text -> IO w -> (w -> a -> IO ()) -> PropertyRowAdder a
mkPropertyRowReadOnly name mkWidget onRefChange row grid _ = do
    label  <- Gtk.labelNewWithMnemonic (Just name)
    widget <- mkWidget
    Gtk.gridAttach grid label 0 row 1 1
    Gtk.gridAttach grid widget 1 row 1 1
    pure $ onRefChange widget

----------------------------------
-- IsEditableProperty instances --
----------------------------------

instance (w ~ Gtk.Entry) => IsEditableProperty Text w ref where
    mkRowWith mutability action name textLens = case mutability of
        MutableRow  -> mkPropertyRow name (`on` #changed) mkWidget onWidgetChange onRefChange
        ReadOnlyRow -> mkPropertyRowReadOnly name mkWidget onRefChange
        where mkWidget =
                  Gtk.entryNew >>=/ action
              onWidgetChange w a = do
                  txt <- get w #text
                  pure (textLens .~ txt $ a)
              onRefChange w a =
                  set w [#text := a^.textLens]

instance (w ~ Gtk.CheckButton) => IsEditableProperty Bool w ref where
    mkRowWith mutability action name boolLens = case mutability of
        MutableRow  -> mkPropertyRow name (`on` #toggled) mkWidget onWidgetToggle onRefChange
        ReadOnlyRow -> mkPropertyRowReadOnly name mkWidget onRefChange
        where mkWidget =
                  Gtk.checkButtonNew >>=/ action
              onWidgetToggle _ a =
                  pure (over boolLens not a)
              onRefChange w a =
                  set w [#active := a^.boolLens]