module Kathu.Parsing.Yaml where

import           Prelude               hiding (null)

import           Control.Lens          (imap)
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.Char             (isSpace)
import           Data.Function         (on)
import qualified Data.HashMap.Strict   as HashMap
import           Data.List             (maximumBy, sortBy)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Vector           (Vector)
import qualified Data.Vector           as Vec
import           Data.Yaml.Builder
import           Data.Yaml.Internal    as YInternal
import           Text.Libyaml

newtype FieldOrder = FieldOrder (Map Text Int)

mkFieldOrder :: [(Text, Int)] -> FieldOrder
mkFieldOrder assocs = FieldOrder $ Map.fromList assocs

mkFieldOrderFromList :: [Text] -> FieldOrder
mkFieldOrderFromList = FieldOrder . Map.fromList . imap (\i t -> (t, i))

projectStringStyle :: Text -> Style
projectStringStyle txt
    | "\n" `T.isInfixOf` txt        = Literal
    | YInternal.isSpecialString txt = DoubleQuoted
    | T.length txt <= 1             = DoubleQuoted -- If only one, likely Char and we should quote it, such as in tile legends
    | hasSurroundingSpace           = DoubleQuoted
    | otherwise                     = PlainNoTag
    where hasSurroundingSpace = isSpace (T.head txt) || isSpace (T.last txt)

projectStringScalar :: Style -> Text -> Event
projectStringScalar _ ""     = EventScalar "" NoTag DoubleQuoted Nothing
projectStringScalar style t  = EventScalar (T.encodeUtf8 t) NoTag style Nothing

-- | Determines if values in a vector are complex and might be serialized into longer expressions
isVectorComplex :: Vector Value -> Bool
isVectorComplex vec = Vec.length vec > 4 || any isComplex vec
    where isComplex val = case val of
              Object _ -> True
              Array  _ -> True
              String t -> T.length t > 30 || "\n" `T.isInfixOf` t
              _        -> False

listStyle :: Vector Value -> Style
listStyle = maximumBy (compare `on` styleOrdVal) . map getStyle . Vec.toList
    where getStyle (String t) = projectStringStyle t
          getStyle _          = DoubleQuoted
          styleOrdVal s = case s of {PlainNoTag -> 1 :: Int; DoubleQuoted -> 2; Literal -> 3; _ -> 0}

-----------------------
-- Pretty Formatters --
-----------------------

prettyList :: Bool -> Vector YamlBuilder -> YamlBuilder
prettyList isComplex bs = YamlBuilder $ (EventSequenceStart NoTag seqType Nothing :) . flip (Vec.foldr unwrap) bs . (EventSequenceEnd:)
  where unwrap (YamlBuilder b) = b
        seqType = if isComplex then AnySequence else FlowSequence

prettyText :: Text -> YamlBuilder
prettyText txt = YamlBuilder (projectStringScalar (projectStringStyle txt) txt :)

prettyTextWith :: Style -> Text -> YamlBuilder
prettyTextWith style txt = YamlBuilder (projectStringScalar style txt :)

prettyMapping :: [(Text, YamlBuilder)] -> YamlBuilder
prettyMapping = maybeNamedMappingComplex Nothing . map (\(k, v) -> (prettyText k, v))

prettyFormat :: Maybe Style -> FieldOrder -> Value -> YamlBuilder
prettyFormat forcedStringStyle fo@(FieldOrder fieldOrd) = go
    where go Null       = null
          go (Bool b)   = bool b
          go (Number n) = scientific n
          go (String s) = case forcedStringStyle of
                              Nothing    -> prettyText s
                              Just style -> prettyTextWith style s
          go (Array a)  = let style = listStyle a
                           in prettyList (isVectorComplex a) (prettyFormat (Just style) fo <$> a)
          go (Object o) = let getOrd t = fromMaybe 1000 $ Map.lookup t fieldOrd
                              sort = sortBy (compare `on` getOrd . fst)
                           in prettyMapping . sort
                            . HashMap.toList
                            . HashMap.map (prettyFormat Nothing fo)
                            . HashMap.filter (/=Null) $ o

encodeYaml :: ToJSON a => a -> ByteString
encodeYaml = toByteString . prettyFormat Nothing (FieldOrder Map.empty) . toJSON

encodeYamlWithFieldOrder :: ToJSON a => FieldOrder -> a -> ByteString
encodeYamlWithFieldOrder fieldOrd = toByteString . prettyFormat Nothing fieldOrd . toJSON