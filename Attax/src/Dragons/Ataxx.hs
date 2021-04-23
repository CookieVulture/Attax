{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Dragons.Ataxx
Description : Ataxx-specific things students don't need to see
Author      : Dhairya Patel
License     : AllRightsReserved

This module collects functions and instances for Ataxx-specific data
structures (so they don't belong in the generic game framework), but
are also implementation details that students don't need to concern
themselves with.
-}
module Dragons.Ataxx where

import AI
import Ataxx
import Data.Aeson
import Dragons.Game

toAITable :: [(String, AIFunc)] -> [(String, GenericAIFunc GameState Move)]
toAITable = (fmap . fmap) toGenericAIFunc
  where
    toGenericAIFunc :: AIFunc -> GenericAIFunc GameState Move
    toGenericAIFunc aiFunc st = case aiFunc of
      NoLookahead f -> [f st]
      WithLookahead f -> map (f st) [1..]

rules1100 :: GameRules GameState Move
rules1100 = GameRules
  { gameInitialState = initialState (7, 7)
  , gameGetTurn = turn
  , gameApplyMove = applyMove
  }

rules1130 :: GameRules GameState Move
rules1130 = rules1100 { gameInitialState = initialState (9, 9) }

-- How to turn Attax's move types to and from JSON. Best practice is
-- to define instances next to either the data type or the
-- typeclass. These are "orphan" instances, and normally poor
-- practice, but we don't want to have too much mysterious code in
-- files that students need to read.

instance FromJSON Move where
  parseJSON = withObject "move" $ \o -> Move
    <$> o .: "from"
    <*> o .: "to"

instance FromJSON Location where
  parseJSON = withObject "location" $ \o -> Location
    <$> o .: "x"
    <*> o .: "y"

instance ToJSON Move where
  toJSON (Move from to) = object
    [ "from" .= from
    , "to" .= to
    ]

instance ToJSON Location where
  toJSON (Location x y) = object
    [ "x" .= x, "y" .= y ]

instance ToJSON GameState where
  toJSON (State t bnd brd h) = object
    [ "turn" .= t
    , "bounds" .= bnd
    , "board" .= jsonBoard brd
    , "history" .= map jsonBoard h
    ]
    where
      jsonBoard :: Board -> Value
      jsonBoard = String . (foldMap . foldMap) (\case
        Piece Player1 -> "1"
        Piece Player2 -> "2"
        Empty -> " "
        Block -> "#")
