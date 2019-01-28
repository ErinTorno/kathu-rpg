{-# LANGUAGE DeriveApplicator #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Parser where

import Data.Map (Map)
import IO.Path

newtype Parser a = Parser {parse :: String -> (a, String)} deriving (Functor, Applicator)

data Expr = Plain String
          | Param String
          | OrElse Expr Expr
          | GetForm String String
          | TagCase [(String, Expr)]

runParser :: Parser a -> Text -> a
runParser (Parser p) t = case p t of
    [(res, [])] -> res
    [(_, [])]   -> error "Parser did not consume entire line"
    _           -> error "Parser encountered an error"