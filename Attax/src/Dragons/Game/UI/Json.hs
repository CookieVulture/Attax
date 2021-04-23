{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Dragons.Game.UI.Json
Description : Emit JSON that describes how the game went
Author      : Dhairya Patel
License     : AllRightsReserved
-}
module Dragons.Game.UI.Json
  ( -- * Overview
    -- $overview
    -- * Types
    GameReport(..)
  , LoggedMove(..)
  , Status(..)
    -- * Entry Point
  , jsonUI
  ) where

import           Control.DeepSeq (NFData)
import           Data.Aeson hiding (Error)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.IORef
import           Dragons.Game
import           Dragons.Game.Network (recv, send)
import           Safe (lastMay)
import           System.Exit (exitSuccess, exitFailure)

-- $overview
--
-- For noninteractive testing, we want an interface that shows what's
-- going on in a machine-readable format. This module provides a
-- common way to do that, provided that the @st@ and @mv@ types are
-- serialisable.

-- | The report we write at the end of the game.
data GameReport st mv = GameReport
  { grStatus :: Status -- ^ How did this game end?
  , grP1Name :: String -- ^ Name of player 1's AI, or "network"
  , grP2Name :: String -- ^ Name of player 2's AI, or "network"
  , grMoveLog :: [LoggedMove mv] -- ^ Log of all moves
  , grFinalState :: st -- ^ Game state at game end
  } deriving (Eq, Show)

instance (ToJSON st, ToJSON mv) => ToJSON (GameReport st mv) where
  toJSON report = object
    [ "gameStatus" .= grStatus report
    , "player1" .= grP1Name report
    , "player2" .= grP2Name report
    , "moves" .= grMoveLog report
    , "finalState" .= grFinalState report
    ]

-- | An entry in the move log.
data LoggedMove mv
  = Normal Player mv -- ^ This move was played.
  | Illegal Player mv -- ^ This move was tried, but the game engine rejected it.
  | NoResponse Player -- ^ We timed out waiting for an AI move.
  deriving (Eq, Show)

instance ToJSON mv => ToJSON (LoggedMove mv) where
  toJSON loggedMove = case loggedMove of
    Normal p mv -> object
      [ "$type" .= String "normal"
      , "player" .= p
      , "move" .= mv
      ]
    Illegal p mv -> object
      [ "$type" .= String "illegal"
      , "player" .= p
      , "move" .= mv
      ]
    NoResponse p -> object
      [ "$type" .= String "noResponse"
      , "player" .= p
      ]

-- | How did the game finish?
data Status
  = Complete -- ^ We ran to a conclusion under the game's rules.
  | Error -- ^ We were stopped before the game end by an error.
  deriving (Eq, Show)

instance ToJSON Status where
  toJSON status = case status of
    Complete -> "complete"
    Error -> "error"

-- | Construct a noninteractive 'GameUI' that emits JSON. It will fail
-- if either player's 'MoveSource' is 'Human'.
jsonUI
  :: forall st mv
   . (FromJSON mv, NFData mv, ToJSON mv, ToJSON st)
  => GameConfig st mv
  -> IO (GameUI st mv)
jsonUI config = do
  stateRef <- newIORef Nothing
  moveLogRef <- newIORef []
  let
    askMove :: Player -> st -> Maybe (Player, mv) -> IO mv
    askMove p st _ = do
      writeIORef stateRef (Just st)

      -- Get the move from our source
      mv <- case configMoveSource config p of
        Human -> error "jsonUI: human player not supported"
        AI _ aiFunc -> do
          mvs <- forceListWithTimeout (configAITimeout config) (aiFunc st)
          case lastMay mvs of
            Nothing -> do
              modifyIORef moveLogRef (NoResponse p:)
              BL.putStrLn . encode =<< makeReport Error st
              exitFailure
            Just mv -> pure mv
        Network gs -> recv gs

      -- Send the move to the other player, if needed
      case configMoveSource config (otherPlayer p) of
        Network gs -> send gs mv
        _ -> pure ()

      modifyIORef moveLogRef (Normal p mv:)
      pure mv

    illegal :: Player -> mv -> IO ()
    illegal p mv = do
      -- We pushed the illegal move to the log, so remove it and
      -- replace it with one that's illegal.
      modifyIORef moveLogRef ((Illegal p mv:) . tail)
      st <- readIORef stateRef
      BL.putStrLn . encode =<< makeReport Error st
      exitFailure

    final _ st = do
      BL.putStrLn . encode =<< makeReport Complete st
      exitSuccess

    makeReport status st = do
      moves <- reverse <$> readIORef moveLogRef
      pure $ GameReport
        { grStatus = status
        , grP1Name = moveSourceName $ configMoveSource config Player1
        , grP2Name = moveSourceName $ configMoveSource config Player2
        , grMoveLog = moves
        , grFinalState = st
        }

    moveSourceName :: MoveSource st mv f -> String
    moveSourceName source = case source of
      Human -> "human"
      AI name _ -> "ai:" ++ name
      Network _ -> "network"

  pure $ GameUI
    { uiInitialUpdate = const $ pure ()
    , uiAskMove = askMove
    , uiFinalUpdate = final
    , uiIllegalMove = illegal
    }
