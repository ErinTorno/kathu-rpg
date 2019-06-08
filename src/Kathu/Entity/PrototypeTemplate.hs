{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.Entity.PrototypeTemplate (defineData, defineEntityCreator, defineEntityFromJSON) where

import Apecs
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Char
import Data.Functor.Compose
import Data.Maybe (maybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Kathu.IO.Parsing
import Kathu.Util.Misc ((>>>=), toCamelCase)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- This modules contains functions that generating helper functions for EntityPrototypes from a given list of components

defBang = Bang NoSourceUnpackedness NoSourceStrictness

-- makes the field name use the prefix and proto
-- ex: "my" -> "Position" -> "myProtoPosition"
fieldName :: String -> Name -> Name
fieldName prefix name = mkName . toCamelCase $ prefix <> nameBase name

defineData :: String -> String -> [Name] -> Q [Dec]
defineData typename prefix names = pure . pure $ DataD [] (mkName typename) [] Nothing [construct] [derivingClaus]
    where mkField name  = (fieldName prefix name, defBang, (ConT ''Maybe `AppT` ConT name))
          construct     = RecC (mkName typename) . map mkField $ names
          derivingClaus = DerivClause Nothing . fmap (ConT) $ [''Generic]

defineEntityCreator :: String -> String -> [Name] -> Q [Dec]
defineEntityCreator fnName prefix names = do
    let param   = [VarP . mkName $ "proto"]
    lambdaExpr <- [| (maybe (pure ()) (set ety) . f $ proto) >> pure ety |]
    let lambdaSet         = LamE [VarP . mkName $ "f", VarP . mkName $ "ety"] lambdaExpr
        applyFor acc name = UInfixE acc (VarE '(>>=)) $ AppE lambdaSet . VarE $ fieldName prefix name
    newEnt     <- [| newEntity () |]
    let body    = UInfixE (foldl applyFor newEnt names) (VarE '(>>=)) (VarE 'pure)
    pure . pure $ FunD (mkName fnName) [Clause param (NormalB body) []]

defineEntityFromJSON :: Name -> Name -> Name -> String -> String -> [Name] -> [Name] -> Q [Dec]
defineEntityFromJSON lib etyLens getID name prefix names slNames = pure . pure $ InstanceD Nothing [] typeSigna [pJSON]
    where typename     = mkName name
          typenameLit  = LitE . StringL $ name
          typeSigna    = AppT (ConT ''FromJSON) . ParensT . AppT (AppT (ConT ''SystemLink) (ConT lib)) $ (ConT typename)
          -- now in pair, where if type requires FromJSON SystemLink, then snd is True
          (first: rem) = (\n -> (n, n `elem` slNames)) <$> names
          varName      = mkName "v"

          indvParse (compName, isLinked) = UInfixE (VarE varName) (VarE (if isLinked then '(.:~?) else '(.:^?))) (LitE . StringL . camelTo2 '-' . show $ name)
              where name = fieldName prefix compName
          sucExpr      = foldl (\acc cur -> UInfixE acc (VarE '(<*>)) (indvParse cur)) (UInfixE (ConE typename) (VarE '(<$>)) (indvParse first)) rem

          protoName    = mkName "inproto"
          succBody     = UInfixE (VarE 'getCompose) (VarE '($)) sucExpr
          -- after we have parsed the Parsing (SystemLink lib EntityPrototype), we then pass it into this
          applyBody    = let nm = mkName "etyproto"
              in UInfixE (VarE protoName) (VarE '(>>>=)) (LamE [VarP nm] $ AppE (AppE (AppE (VarE 'insertSL) (VarE etyLens)) (AppE (VarE getID) (VarE nm))) (VarE nm))
          body         = AppE (AppE (VarE 'withObject) typenameLit) $ LamE [VarP varName] (LetE [ValD (VarP protoName) (NormalB succBody) []] applyBody)
          parseSucc    = Clause [] (NormalB body) []
          pJSON        = FunD ('parseJSON) [parseSucc]