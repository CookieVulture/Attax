{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Dragons.Game
Description : General framework for 2-player games
Author      : Dhairya Patel
License     : AllRightsReserved

Abstract framework for a two-player, turn-based, perfect information
game.
-}
module Dragons.Game
  ( -- * Overview
    -- $overview
    -- * Game
    -- ** Types
    Player(..)
  , player
  , otherPlayer
  , Turn(..)
  , Outcome(..)
  , MoveSource(..)
  , DebugFlag(..)
  , GameRules(..)
  , GameConfig(..)
  , GameUI(..)
    -- ** Entry Point
  , runGame
    -- * AI
    -- ** Types
  , GenericAIFunc
  , Seconds
    -- ** Helper Functions
  , aiFailedToMove
  , aiIllegalMove
  , networkIllegalMove
  , printLookaheadTrace
  , forceListWithTimeout
  ) where

import Ataxx (Player(..), otherPlayer, Outcome(..), Turn(..))
import Control.DeepSeq (NFData(..), force)
import Control.Monad.State (StateT, runStateT, get, gets, put)
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Dragons.Game.Network (GameSocket)
import System.Timeout (timeout)

-- $overview
--
-- A running game has two parts: the /game rules/, which controls how
-- the game evolves from turn to turn, and the /game ui/, which asks
-- the players (who may be local, remote, or bots) for moves. Both are
-- parameterised over a type @st@ of /game states/ and a type @mv@ of
-- /moves/.

-- | The catamorphism (fold) for 'Player'.
player :: a -> a -> Player -> a
player p1 p2 p = case p of
  Player1 -> p1
  Player2 -> p2

-- | Where our moves come from. Each UI framework will interpret this
-- its own way in its implementation of 'Dragons.Game.uiAskMove'.
--
-- 'MoveSouce' is parameterised by @f@ so that we can reuse the
-- structure for option-parsing and running games. When parsing
-- options, we use @f ~ 'Data.Proxy.Proxy'@ so that we don't have to
-- provide a socket. When running the game, we use @f ~ 'IORef'@,
-- which means that the socket is available.
--
-- This pattern is called [higher-kinded
-- data](https://github.com/qfpl/applied-fp-course/blob/711a3bcbb4882c247e474542a239092ab24b52ee/bonus/hkd.md).
data MoveSource st mv f
  = Human -- ^ A local human at this computer
  | AI String (GenericAIFunc st mv) -- ^ A bot running on this computer
  | Network (f GameSocket) -- ^ The move will come from the network.

-- | Debugging flags.
data DebugFlag = DebugLookahead deriving (Eq, Show)

-- | Collection of callback functions that describe the rules of a
-- game.
data GameRules st mv = GameRules
  { gameInitialState :: st -- ^ The state of the game at game start.
  , gameGetTurn :: st -> Turn -- ^ Whose turn is it?
  , gameApplyMove :: mv -> st -> Maybe st
    -- ^ Apply a move to the game state. Return 'Nothing' if the move
    -- was illegal.
  }

-- | Configuration data that's independent of UIs.
data GameConfig st mv = GameConfig
  { configMoveSource :: Player -> MoveSource st mv IORef
    -- ^ Where moves come from.
  , configAITimeout :: Seconds -- ^ How long to wait for AI players.
  , configDebugFlags :: [DebugFlag] -- ^ Debugging flags.
  }

-- | Collection of callback functions for a game's UI.
data GameUI st mv = GameUI
  { uiInitialUpdate :: st -> IO () -- ^ Called when the game starts.
  , uiAskMove :: Player -> st -> Maybe (Player, mv) -> IO mv
    -- ^ Ask for a player's next move. @Maybe ('Player', mv)@ is the
    -- previous move.
  , uiFinalUpdate :: Outcome -> st -> IO () -- ^ Called when the game is over.
  , uiIllegalMove :: Player -> mv -> IO ()
    -- ^ Called when the submitted move was illegal. Will be followed
    -- by either 'uiAskP1Move' or 'uiAskP2Move'.
  }

-- | Actually run a game, by providing the rules and a UI for the same
-- game. Roughly speaking:
--
-- 1. Give the UI the initial state.
--
-- 2. Until the game is over, ask the UI for the current player's
--    move, and apply it to the game. If the move was illegal, say so
--    and ask again.
--
-- 3. Give the UI the final state.
runGame :: forall st mv . GameRules st mv -> GameUI st mv -> IO ()
runGame rules ui = do
  let initialSt = gameInitialState rules
  uiInitialUpdate ui initialSt
  (outcome, (finalSt, _)) <- runStateT gameLoop (initialSt, Nothing)
  uiFinalUpdate ui outcome finalSt

  where
    gameLoop :: StateT (st, Maybe (Player, mv)) IO Outcome
    gameLoop = do
      t <- currentTurn
      case t of
        Turn p -> nextMove p *> gameLoop
        GameOver o -> pure o

    currentTurn :: StateT (st, Maybe (Player, mv)) IO Turn
    currentTurn = gets (gameGetTurn rules . fst)

    nextMove :: Player -> StateT (st, Maybe (Player, mv)) IO ()
    nextMove p = do
      (st, mLastMove) <- get
      mv <- askMove st mLastMove
      case gameApplyMove rules mv st of
        Nothing -> lift (uiIllegalMove ui p mv) *> nextMove p
        Just st' -> put (st', Just (p, mv))

      where
        askMove s mLast = lift $ uiAskMove ui p s mLast

-- | An 'GenericAIFunc' is how bots decide what move to make. Given a
-- game state @st@, return a sequence of increasingly good moves @mv@
-- (e.g., by increasing lookahead). The returned list of moves will be
-- forced until either it times out, or the final element of the list
-- is reached.
type GenericAIFunc st mv = st -> [mv]

-- | Type of timeouts.
type Seconds = Double

-- | Signal that an AI failed to move.
aiFailedToMove :: Player -> String -> a
aiFailedToMove p name =
  error $ "AI (" <> show p <> ", " <> name <> ") failed to move."

-- | Signal that an AI has made an illegal move.
aiIllegalMove :: Player -> String -> a
aiIllegalMove p name =
  error $ "AI (" <> show p <> ", " <> name <> ") made an illegal move."

-- | Signal that an illegal move came in over the network.
networkIllegalMove :: Player -> a
networkIllegalMove p = error $ show p <> " (network) made an illegal move."

-- | Print out a list of moves considered by a lookahead function.
printLookaheadTrace :: Show mv => Player -> String -> [mv] -> IO ()
printLookaheadTrace p name mvs = putStrLn $ concat
  [ "AI ("
  , show p
  , ", "
  , show name
  , ") looked ahead up to "
  , show (length mvs)
  , " moves.\n"
  , "Moves considered: " ++ show mvs
  ]

-- | Given a timeout, walk the list, forcing each @a@ in turn. Return
-- the as much of the list as we got to before we ran out of time.
forceListWithTimeout :: NFData a => Seconds -> [a] -> IO [a]
forceListWithTimeout time lazyList = do
  results <- newIORef []

  let
    walk list = case list of
      [] -> pure ()
      x:xs -> force x `seq` do
        modifyIORef results (x:)
        walk xs

    timeLimit = round (time * 1000000)
  _ <- timeout timeLimit (walk lazyList)

  -- We built the list up backwards
  reverse <$> readIORef results

-- Orphan instances for types that really should belong in this
-- module, but that we define alongside the game for the students'
-- benefit.

instance ToJSON Player where
  toJSON p = case p of
    Player1 -> "player1"
    Player2 -> "player2"

instance ToJSON Outcome where
  toJSON o = case o of
    Winner p -> object
      [ "$type" .= String "winner"
      , "player" .= p
      ]
    Draw -> object
      [ "$type" .= String "draw"
      ]

instance ToJSON Turn where
  toJSON t = case t of
    Turn p -> object
      [ "$type" .= String "turn"
      , "player" .= p
      ]
    GameOver o -> object
      [ "$type" .= String "gameOver"
      , "outcome" .= o
      ]
