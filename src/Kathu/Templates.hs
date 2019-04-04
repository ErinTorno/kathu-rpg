{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.Templates where

import Control.Monad (replicateM)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

defineLiftA :: Int -> Q [Dec]
defineLiftA n | n > 3 = do
    (x:xs)  <- replicateM n $ newName "x"
    let f    = mkName "f"
        name = mkName ("liftA" ++ show n) 
        body = foldl (\acc x -> UInfixE acc (VarE '(<*>)) x) (UInfixE (VarE f) (VarE '(<$>)) (VarE x)) . map VarE $ xs
        args = map VarP (f:(x:xs))
    pure . pure $ FunD name [Clause args (NormalB body) []]
              | otherwise = pure []