{-# LANGUAGE OverloadedStrings #-}

module Dragons.Main where

import AI (ais)
import Data.IORef (IORef)
import Data.Proxy (Proxy)
import Data.Streaming.Network (bindPortTCP, getSocketTCP)
import Dragons.Ataxx (toAITable, rules1100, rules1130)
import Dragons.Ataxx.CodeWorld (codeWorldUI)
import Dragons.Ataxx.Text (textUI)
import Dragons.Game (GameConfig(..), MoveSource(..), player, runGame)
import Dragons.Game.Network (mkGameSocket)
import Dragons.Game.UI.Json (jsonUI)
import Dragons.Options
import Network.Socket (accept, close)

appMain :: IO ()
appMain = do
  options <- parseOptions (toAITable ais)

  let
    -- Only print when running in interactive mode
    putStrLnInteractive s = case optUI options of
      Json -> pure ()
      _ -> putStrLn s

    addSocketToSource
      :: MoveSource st mv Proxy
      -> IO (MoveSource st mv IORef)
    addSocketToSource source = case source of
      Human -> pure Human
      AI name f -> pure $ AI name f
      Network _ -> case optNetworkMode options of
        Nothing -> error "appMain: Network move source but no network config."
        Just (Host port) -> do
          server <- bindPortTCP port "*"
          putStrLnInteractive "Waiting for other player to connect..."
          (s, _) <- accept server
          close server
          Network <$> mkGameSocket s
        Just (Connect host port) -> do
          putStrLnInteractive "Connecting to remote game..."
          (s, _) <- getSocketTCP host port
          Network <$> mkGameSocket s

  -- Set up the network sockets, if we need to.
  p1Source <- addSocketToSource $ optPlayer1 options
  p2Source <- addSocketToSource $ optPlayer2 options

  let
    config = GameConfig
      { configMoveSource = player p1Source p2Source
      , configAITimeout = optTimeout options
      , configDebugFlags = optDebugFlags options
      }

    rules = case optMode options of
      COMP1100 -> rules1100
      COMP1130 -> rules1130

  -- Launch the UI that the command-line asked for.
  case optUI options of
    Text -> runGame rules $ textUI config
    CodeWorld -> runGame rules =<< codeWorldUI config
    Json -> runGame rules =<< jsonUI config
