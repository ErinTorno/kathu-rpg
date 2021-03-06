{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.PrototypeTemplate (defineData, defineEntityCreator, defineEntityFromJSON) where

import           Apecs
import           Data.Aeson
import           Data.Char                          (toLower)
import           Data.Functor.Compose
import           Data.List                          (nub)
import           Language.Haskell.TH

import           Kathu.Entity.Components            (newExistingEntity)
import           Kathu.Entity.SerializableComponent
import           Kathu.Parsing.Aeson                ((.:~?), (.:^?))
import           Kathu.Util.Dependency
import           Kathu.Util.Flow                    ((>>>=))
import           Kathu.Util.Types                   (IDMap)

-- This modules contains functions that generating helper functions for EntityPrototypes from a given list of components

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

-- makes the field name use the prefix and proto
-- ex: "my" -> "Position" -> "myProtoPosition"
-- we filter out ' as those usually signify a more specified type, and we'd like to just use the generic for naming
fieldName :: String -> Name -> Name
fieldName prefix name = mkName . toCamelCase . filter (/='\'') $ prefix <> nameBase name
    where toCamelCase [] = []
          toCamelCase (c:cs) = toLower c : cs

-- MyType -> ["m", "a"] -> MyType m a
applyParams :: Type -> [String] -> Type
applyParams typ = ParensT . foldl (\acc cur -> AppT acc (VarT . mkName $ cur)) typ

-- | Creates a record type for an entity prototype with the given components
defineData :: String -> [SerializableComponent] -> Q [Dec]
defineData typename components = pure . pure $ DataD [] (mkName typename) uniqueParams Nothing [construct] []
    where mkField comp  = let name = compName comp in (fieldName "" name, defaultBang, AppT (ConT ''Maybe) . applyParams (ConT name) . params $ comp)
          uniqueParams  = map (PlainTV . mkName) . nub . concatMap params $ components
          construct     = RecC (mkName typename) . map mkField $ components

-- | Creates a function that will create a new entity in the world from the given record type
defineEntityCreator :: String -> [SerializableComponent] -> Q [Dec]
defineEntityCreator fnName components = do
    let param   = [VarP . mkName $ "proto"]
    lambdaExpr <- [| (maybe (pure ()) (set ety) . f $ proto) >> pure ety |]
    let lambdaSet         = LamE [VarP . mkName $ "f", VarP . mkName $ "ety"] lambdaExpr
        applyFor acc name = UInfixE acc (VarE '(>>=)) $ AppE lambdaSet . VarE $ fieldName "" name
    newEnt     <- [| newExistingEntity () |]
    let body    = UInfixE (foldl applyFor newEnt . map compName $ components) (VarE '(>>=)) (VarE 'pure)
    pure . pure $ FunD (mkName fnName) [Clause param (NormalB body) []]

-- | Defines FromJSON instances for a entity prototype
defineEntityFromJSON :: Name -> Name -> [SerializableComponent] -> Q [Dec]
defineEntityFromJSON getID typename components = pure . pure $ InstanceD Nothing contraints typeSignature [pJSON]
    where stateVar     = VarT . mkName $ "s"
          monadVar     = VarT . mkName $ "m"
          uniqueParams = nub . concatMap params $ components
          typenameLit  = LitE . StringL . nameBase $ typename
          mkDependency = ParensT . AppT (AppT (AppT (ConT ''Dependency) stateVar) monadVar)

          typeNameWithParams = flip applyParams uniqueParams . ConT $ typename
          typeSignature      = AppT (ConT ''FromJSON) . ParensT . AppT (AppT (AppT (ConT ''Dependency) stateVar) monadVar) $ typeNameWithParams

          mkConstraints f    = foldl (\acc cur -> AppT (ConT ''FromJSON) (f (applyParams (ConT . compName $ cur) (params cur))) : acc) []
          constraintsNoDep   = mkConstraints id           . filter (not . requiresDependencies) $ components
          constraintsDeps    = mkConstraints mkDependency . filter requiresDependencies         $ components
          constraintMonad    = AppT (ConT ''Monad) monadVar
          constraintStore    = AppT (AppT (ConT ''CanStore) stateVar) (ParensT . AppT (ConT ''IDMap) $ typeNameWithParams)
          contraints         = constraintMonad : constraintStore : (constraintsNoDep ++ constraintsDeps)
          -- now in pair, where if type requires FromJSON Dependency, then snd is True
          compDepPairs = (\c -> (compName c, requiresDependencies c)) <$> components
          varName      = mkName "v"

          indvParse (cName, isLinked) = UInfixE (VarE varName) (VarE (if isLinked then '(.:~?) else '(.:^?))) (LitE . StringL . camelTo2 '-' . show $ combinedName)
              where combinedName = fieldName "" cName
          sucExpr      = foldl (\acc cur -> UInfixE acc (VarE '(<*>)) (indvParse cur)) (UInfixE (ConE . mkName . nameBase $ typename) (VarE '(<$>)) (indvParse $ head compDepPairs)) (tail compDepPairs)

          protoName    = mkName "inproto"
          succBody     = UInfixE (VarE 'getCompose) (VarE '($)) sucExpr
          -- after we have parsed the Parsing (Dependency s m EntityPrototype), we then pass it into this
          applyBody    = UInfixE (VarE protoName) (VarE '(>>>=)) (AppE (VarE 'storeWithKeyFn) (VarE getID))
          body         = AppE (AppE (VarE 'withObject) typenameLit) $ LamE [VarP varName] (LetE [ValD (VarP protoName) (NormalB succBody) []] applyBody)
          parseSucc    = Clause [] (NormalB body) []
          pJSON        = FunD 'parseJSON [parseSucc]