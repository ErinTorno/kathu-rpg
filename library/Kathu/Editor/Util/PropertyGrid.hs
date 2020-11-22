{-# LANGUAGE AllowAmbiguousTypes #-}

module Kathu.Editor.Util.PropertyGrid where
    
import           Control.Lens               hiding (set)
import           Control.Monad              (void)
import           Data.GI.Base
import           Data.Int
import           Data.IORef
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vec
import qualified GI.Gtk                     as Gtk
import           Linear.V2
import           Text.Read                  (readMaybe)

import           Kathu.Editor.Types
import           Kathu.Editor.Util.GtkMisc
import           Verda.Util.Flow            ((>>=/))
import           Verda.Util.Types

type PropertyRowAdder a = Int32 -> Gtk.Grid -> IORef a -> IO (EditableProperty a)

data RowMutability = MutableRow | ReadOnlyRow deriving (Show, Eq)

class ParsableProperty a where
    propertyFromText :: Text -> Maybe a
    showProperty     :: a -> Text
    default showProperty :: Show a => a -> Text
    showProperty = T.pack . show

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
    widget <- mkWidget
    void . onSignalChange widget $
        readIORef ref >>= onWidgetChange widget >>= writeIORef ref

    attachPropertyRowToGrid grid row name widget
    pure $ onRefChange widget

mkPropertyRowReadOnly :: Gtk.IsWidget w => Text -> IO w -> (w -> a -> IO ()) -> PropertyRowAdder a
mkPropertyRowReadOnly name mkWidget onRefChange row grid _ = do
    widget <- mkWidget
    attachPropertyRowToGrid grid row name widget
    pure $ onRefChange widget

attachPropertyRowToGrid :: Gtk.IsWidget w => Gtk.Grid -> Int32 -> Text -> w -> IO ()
attachPropertyRowToGrid grid row name widget = do
    label  <- Gtk.labelNewWithMnemonic (Just name)
    set label [#halign := Gtk.AlignStart]
    Gtk.gridAttach grid label 0 row 1 1
    Gtk.gridAttach grid widget 1 row 1 1

----------------------------------
-- IsEditableProperty instances --
----------------------------------

instance (w ~ Gtk.Entry) => IsEditableProperty Text w ref where
    mkRowWith mutability action name textLens = case mutability of
        MutableRow  -> mkPropertyRow name (`on` #changed) mkWidget onWidgetChange onRefChange
        ReadOnlyRow -> mkPropertyRowReadOnly name (mkWidget >>=/ \w -> set w [#editable := False, #canFocus := False]) onRefChange
        where mkWidget =
                  Gtk.entryNew >>=/ action
              onWidgetChange w a = do
                  txt <- get w #text
                  pure (textLens .~ txt $ a)
              onRefChange w a =
                  set w [#text := a^.textLens]

instance (w ~ Gtk.Entry) => IsEditableProperty Identifier w ref where
    mkRowWith mutability action name idLens = mkRowWith mutability action name (idLens . textIDLens)

instance (w ~ Gtk.CheckButton) => IsEditableProperty Bool w ref where
    mkRowWith mutability action name boolLens = case mutability of
        MutableRow  -> mkPropertyRow name (`on` #toggled) mkWidget onWidgetToggle onRefChange
        ReadOnlyRow -> mkPropertyRow name (`on` #toggled) mkWidget onWidgetReverseToggle onRefChange
        where mkWidget =
                  Gtk.checkButtonNew >>=/ action
              onWidgetToggle _ a =
                  pure (over boolLens not a)
              onWidgetReverseToggle w a = do
                  isActive <- get w #active
                  set w [#active := not isActive]
                  pure a
              onRefChange w a =
                  set w [#active := a^.boolLens]

instance (ParsableProperty a, w ~ (Gtk.Entry, Gtk.Entry)) => IsEditableProperty (V2 a) w ref where
    mkRowWith mutability action name vecLens = mkVecPropertyRow
        where mkVecPropertyRow row grid ref = do
                  widget <- mkWidget ref
                  attachPropertyRowToGrid grid row name widget
                  pure $ onRefChange widget
              mkWidget :: IORef ref -> IO Gtk.Box
              mkWidget ref = do
                  xEntry <- mkParsableEntry (propertyFromText @a)
                  yEntry <- mkParsableEntry (propertyFromText @a)
                  action (xEntry, yEntry)
                  case mutability of
                      ReadOnlyRow -> do
                          set xEntry [#editable := False, #canFocus := False]
                          set yEntry [#editable := False, #canFocus := False]
                      MutableRow -> do
                          void . on xEntry #changed $
                              readIORef ref >>= onEntryChange _x xEntry >>= writeIORef ref
                          void . on yEntry #changed $
                              readIORef ref >>= onEntryChange _y yEntry >>= writeIORef ref
                  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
                  set xEntry [#widthChars := 10]
                  set yEntry [#widthChars := 10]
                  Gtk.containerAdd box xEntry
                  Gtk.containerAdd box yEntry
                  pure box
              onRefChange w a = do
                  let V2 x y = a^.vecLens
                  (xEntry, yEntry) <- getEntries w
                  set xEntry [#text := showProperty x]
                  set yEntry [#text := showProperty y]
              onEntryChange sideLens entry a = do
                  txt <- get entry #text
                  let newVal = case propertyFromText @a txt of
                          Just v  -> v
                          Nothing -> error $ "Failed to parse text from entry; mkParsableEntry allowed invalid input " ++ show txt
                  pure $ (vecLens . sideLens .~ newVal) a
              getEntries w = do
                  [xw, yw] <- Gtk.containerGetChildren w
                  xEntry   <- unsafeCastTo Gtk.Entry xw
                  yEntry   <- unsafeCastTo Gtk.Entry yw
                  pure (xEntry, yEntry)

--------------------------------
-- ParsableProperty instances --
--------------------------------

instance ParsableProperty Double where
    propertyFromText txt
        | T.null txt             = Just 0 -- if blank, just assume zero
        | "." `T.isPrefixOf` txt = readMaybe . T.unpack $ T.cons '0' txt -- allow decimals to be at the beginning or the end
        | "." `T.isSuffixOf` txt = readMaybe . T.unpack $ T.snoc txt '0'
        | otherwise              = readMaybe . T.unpack $ txt