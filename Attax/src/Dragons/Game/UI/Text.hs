{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Dragons.Game.UI.Text
Description : Rapidly generate a text interface for the game framework
Author      : Dhairya Patel
License     : AllRightsReserved
-}
module Dragons.Game.UI.Text
  ( -- * Overview
    -- $overview
    -- * Types
    TextUI(..)
    -- * Entry Point
  , textUI
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Dragons.Game
import Dragons.Game.Network (recv, send)
import Safe (lastMay)
import System.IO (BufferMode(..), hSetBuffering, stdout)

-- $overview
--
-- Text UIs for different games have a lot of similar code. This
-- module abstracts over those similarities, so that you can provide
-- only the text handling details for your game, and get a complete UI
-- for it.

-- | Collection of callback functions to drive a text UI. As with
-- 'GameUI', we parameterise over a game state @st@, a move type @mv@
-- and perform actions in some monad @m@.
data TextUI st mv = TextUI
  { textRenderState :: st -> String
    -- ^ Represent the game state as a string. This should be
    -- something prettier than the 'Show' instance.
  , textReadMove :: st -> Maybe (Player, mv) -> IO mv
    -- ^ Read a move. The previous move, if there is one, is
    -- provided. This needs to be a monadic action, as it may need to
    -- be interactive. (Example: games where a piece can make multiple
    -- steps in a turn.)
  }

-- | Construct a 'GameUI' from a 'TextUI'.
textUI
  :: forall st mv
   . (FromJSON mv, NFData mv, ToJSON mv, Show mv)
  => GameConfig st mv
  -> TextUI st mv
  -> GameUI st mv
textUI config text = GameUI
  { uiInitialUpdate = initial
  , uiAskMove = askMove
  , uiFinalUpdate = final
  , uiIllegalMove = illegal
  }
  where
    -- Respond to the game start.
    initial st = do
      hSetBuffering stdout NoBuffering
      putStrLn "Let the game begin!"
      printSt st

    -- Game's finished, so tell the player about it.
    final outcome st = do
      putStrLn "And the game is over!"
      putStrLn "The final state of the game is:"
      printSt st
      putStrLn $ case outcome of
        Winner Player1 -> "Player 1 wins!"
        Winner Player2 -> "Player 2 wins!"
        Draw -> "It's a draw!"

    -- Ask the current player for a move.
    askMove :: Player -> st -> Maybe (Player, mv) -> IO mv
    askMove p st mLastMove = do
      putStrLn "Current game state:"
      printSt st

      -- Get the move from our source
      mv <- case configMoveSource config p of
        Human -> textReadMove text st mLastMove
        AI name aiFunc -> do
          mvs <- forceListWithTimeout (configAITimeout config) (aiFunc st)

          when (DebugLookahead `elem` configDebugFlags config) $
            printLookaheadTrace p name mvs
          case lastMay mvs of
            Nothing -> aiFailedToMove p name
            Just mv -> pure mv
        Network gs -> recv gs

      -- Send the move to the other player, if needed
      case configMoveSource config (otherPlayer p) of
        Network gs -> send gs mv
        _ -> pure ()

      pure mv

    -- Handle an illegal move.
    illegal :: Player -> mv -> IO ()
    illegal p _ =
      case configMoveSource config p of
        Human -> putStrLn "Sorry, your move was illegal. Try again."
        AI name _ -> aiIllegalMove p name
        Network _ -> networkIllegalMove p

    printSt = putStrLn . textRenderState text
