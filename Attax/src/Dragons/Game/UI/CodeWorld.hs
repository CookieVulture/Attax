-- reactiveOf will complain otherwise, and there's no way to compile
-- without a warning.
{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-|
Module      : Dragons.Game.UI.CodeWorld
Description : Rapidly generate a CodeWorld interface for the game framework
Author      : Dhairya Patel
License     : AllRightsReserved
-}
module Dragons.Game.UI.CodeWorld
  ( -- * Overview
    -- $overview
    -- * Types
    CodeWorldUI(..)
  , UIMode(..)
  , UIResponse
    -- * Entry Point
  , codeWorldUI
  ) where

import           Prelude hiding (filter)

import qualified CodeWorld as CW
import           CodeWorld.Reflex
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.DeepSeq (NFData)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Functor ((<&>), void)
import           Data.Witherable ((<$?>), (<&?>), filter)
import           Dragons.Game
import           Dragons.Game.Network (recv, send)
import           Reflex
import           Safe (lastMay)

-- $overview
--
-- A CodeWorld UI runs in a separate thread to the running game, which
-- communicates with your CodeWorld program by passing extra data into
-- your callbacks, and waiting for you to respond by returning a
-- 'UIResponse' from your update function.

-- | Collection of callbacks you should provide for a CodeWorld game
-- UI. They are a lot like the CodeWorld callbacks used in
-- 'CW.activityOf'; you are expected to provide:
--
-- * an initial model,
--
-- * a view function that renders a picture, and
--
-- * an update function that returns a new model.
--
-- There are additional parameters and results so that your program
-- can read and affect the game state. As with 'GameUI', we
-- parameterise over a game state @st@, and a move type @mv@; but we
-- also parameterise over your UI model type @model@.
data CodeWorldUI st mv model = CodeWorldUI
  { cwInitialModel :: model
  , cwView :: UIMode mv -> st -> model -> Picture
    -- ^ Render to a CodeWorld 'Picture'. As with normal CodeWorld,
    -- you have the UI @model@, but you also have access to the
    -- current 'UIMode' and to the game state.
  , cwUpdate
    :: UIMode mv -> st -> model -> CW.Event -> (model, Maybe (UIResponse mv))
    -- ^ Update the UI @model@ in response to an 'CW.Event'. If you
    -- want to pass a response back to the game return 'Just' with a
    -- 'UIResponse'. Constructors for 'UIResponse' are not exported,
    -- so you will need to inspect the constructors of 'UIMode' to see
    -- whether or not you can create a response right now.
  }

-- | The 'UIMode' type tracks what information the driver is expecting
-- from the UI. If the driver is expecting information from the UI,
-- there will be a function available in the @'UIMode' mv@ argument to
-- 'uiUpdate'. Use it to construct a @'UIResponse' mv@ value, and
-- return that @'UIResponse' mv@ from your 'uiUpdate' function.
data UIMode mv
  = Started
    -- ^ The game is being set up, you don't need to respond with
    -- anything yet.
  | AwaitingMove Player (Maybe (Player, mv)) (mv -> UIResponse mv)
    -- ^ Awaiting a human move from the CodeWorld UI. The previous
    -- move, if there is one, is provided.
  | AIThinking Player String
    -- ^ A local AI player is thinking. The 'String' is the AI name.
  | AIFailedToMove Player String
    -- ^ A local AI player failed to make a move, and the game cannot
    -- continue. The 'String' is the AI name.
  | AIIllegalMove Player String
    -- ^ A local AI player made an illegal move, and the game cannot
    -- continue. The 'String' is the AI name.
  | NetworkIllegalMove Player
    -- ^ The remote player in a network game made an illegal move, and
    -- the game cannot continue.
  | Finished Outcome (UIResponse mv)
    -- ^ The game is over. Return the 'UIResponse' when it's time to
    -- shut down.

-- | Responses from the UI that are sent back to the driver. The
-- constructors are not exported, which forces clients to use the
-- functions provided inside the @'UIMode' mv@ type.
data UIResponse mv
  = Move mv
  | Shutdown

-- | Structure holding triggers which the game uses to fire events
-- inside the Reflex network.
data CodeWorldTriggers st mv = CodeWorldTriggers
  { triggerMode :: UIMode mv -> IO ()
  , triggerState :: st -> IO ()
  }

-- | Construct a 'GameUI' from a 'CodeWorldUI'.
codeWorldUI
  :: forall st mv model
   . (FromJSON mv, NFData mv, ToJSON mv, Show mv)
  => GameConfig st mv
  -> CodeWorldUI st mv model
  -> IO (GameUI st mv)
codeWorldUI config cw = do
  -- An MVar is one of Haskell's concurrency primitives, representing
  -- a box that can hold zero or one items. Blocking reads and writes
  -- allow rendezvous between threads.

  -- Triggers that the main thread uses to fire events in the CodeWorld program.
  -- Writer: CodeWorld thread; Reader: main thread.
  mvTriggers :: MVar (CodeWorldTriggers st mv) <- liftIO newEmptyMVar

  -- Initial state of the game.
  -- Writer: main thread; Reader: CodeWorld thread.
  mvInitialState :: MVar st <- liftIO newEmptyMVar

  -- Moves entered by the human player.
  -- Writer: CodeWorld thread; Reader: main thread.
  mvMove :: MVar mv <- liftIO newEmptyMVar

  -- CodeWorld is finished, and asking the main program to shut down.
  -- Writer: CodeWorld thread; Reader: main thread.
  mvShutdown :: MVar () <- liftIO newEmptyMVar

  -- Fork off a thread to run CodeWorld.
  void . liftIO . forkIO $
    -- Unfortunately, I have to use the reflex interface here, as it's
    -- the only way that CodeWorld can do IO.
    --
    -- You might find these videos helpful to get your head around the paradigm:
    -- https://www.youtube.com/watch?v=GXW1jBijhlk
    --
    -- https://www.youtube.com/watch?v=dNBUDAU9sv4
    -- Slides: https://mightybyte.net/real-world-reflex/index.htmpl
    --
    -- The Queensland FP Lab also wrote some introductory material, as
    -- well as a workshop:
    -- https://qfpl.io/projects/reflex.html
    -- https://github.com/qfpl/reflex-workshop/
    reactiveOf $ mdo
      -- An `Event t a` is like a signal wire of `a`s; we can filter
      -- it, map over it, or connect it to other parts of our program.
      --
      -- `fireMode` and `fireState` are the triggers for these events,
      -- which are how the main thread signals changes to the
      -- CodeWorld program.
      eCW <- getCodeWorldEvent -- All CodeWorld events

      (eMode :: Event _ (UIMode mv), fireMode) <- newTriggerEventWithOnComplete
      (eSt :: Event _ st, fireState) <- newTriggerEventWithOnComplete

      -- A `Dynamic t a` is like a memory cell that holds an `a` at
      -- every point in time, and can signal to other parts of the
      -- program when it changes.
      --
      -- `holdDyn` takes an initial value and an event. It creates a
      -- dynamic with the initial value, and replaces it with the
      -- value of the event each time that event fires. The semantics
      -- of FRP guarantee that this happens consistently and
      -- atomically, which is the big payoff of this strange paradigm.
      --
      -- This gives us memory cells for the current UI model, UI mode,
      -- and game state.
      dModel :: Dynamic _ model <- holdDyn (cwInitialModel cw) eNewModel
      dMode :: Dynamic _ (UIMode mv) <- holdDyn Started eMode
      dSt :: Dynamic _ st <- holdDyn st eSt

      let
        -- `Dynamic t` is an `Applicative`, so we can lift the UI's
        -- update callback across our `Dynamic`s. We can then combine
        -- this with the firings of the `eCW` CodeWorld event to get
        -- an `Event` which contains our UI updates.
        eUpdate :: Event _ (model, Maybe (UIResponse mv))
        eUpdate = current (cwUpdate cw <$> dMode <*> dSt <*> dModel) <@> eCW

        -- `Event t` is a `Functor`, so we can split the tuple.
        eNewModel :: Event _ model
        eNewModel = fst <$> eUpdate

        -- `Event t` is also a `Filterable`, which is like a `Functor`
        -- which can discard elements with a `mapMaybe` (aka `(<$?>)`)
        -- function:
        -- (<$?>) :: Filterable f => (a -> Maybe b) -> f a -> f b
        eResponse :: Event _ (UIResponse mv)
        eResponse = snd <$?> eUpdate

        -- `eMove` and `eShutdown` use `mapMaybe` to split the
        -- constructors of `UIResponse`. (As `(&) = flip ($)` and
        -- `(<&>) = flip (<$>)`, so too is `(<&?>) = flip (<$?>)`.)
        eMove :: Event _ mv
        eMove = eResponse <&?> \case
          Move m -> Just m
          _ -> Nothing

        eShutdown :: Event _ ()
        eShutdown = eResponse <&?> \case
          Shutdown -> Just ()
          _ -> Nothing

      -- When the "move" or "shutdown" events happen, populate the
      -- respective MVar so that the main program knows about it.
      performEvent_ $ eMove <&> (liftIO . putMVar mvMove)
      performEvent_ $ eShutdown <&> (liftIO . putMVar mvShutdown)

      -- Draw takes a `Dynamic t Picture` and redraws it on the canvas
      -- whenever it changes. We use the `Applicative` instance of
      -- `Dynamic t` to lift the view callback across our data.
      draw $ cwView cw <$> dMode <*> dSt <*> dModel

      -- Hand off the triggers for our events to the main thread.
      liftIO . putMVar mvTriggers $ CodeWorldTriggers
        { triggerMode = makeSynchronous fireMode
        , triggerState = makeSynchronous fireState
        }

      -- Read the initial state of the game from the main thread.
      -- This isn't strictly necessary (we can get all the data we
      -- need from the event firings), but working with `Dynamic t st`
      -- is much more convenient than `Dynamic t (Maybe st)`.
      st <- liftIO $ takeMVar mvInitialState
      pure ()

  -- Meanwhile, back in the main thread: read the event triggers from our MVar.
  CodeWorldTriggers
    { triggerMode = mode
    , triggerState = state
    } <- takeMVar mvTriggers

  let
    -- Respond to the game start. Put it in the MVar and also fire off
    -- the event trigger.
    initial :: st -> IO ()
    initial st = putMVar mvInitialState st *> state st

    -- Ask the CodeWorld interface for a move.
    askMove :: Player -> st -> Maybe (Player, mv) -> IO mv
    askMove p st mLastMove = do
      state st

      -- Get the move from our source.
      mv <- case configMoveSource config p of
        Human -> do
          mode $ AwaitingMove p mLastMove Move
          takeMVar mvMove
        AI name aiFunc -> do
          mode $ AIThinking p name
          mvs <- forceListWithTimeout (configAITimeout config) (aiFunc st)

          when (DebugLookahead `elem` configDebugFlags config) $
            printLookaheadTrace p name mvs
          case lastMay mvs of
            Nothing -> do
              mode $ AIFailedToMove p name
              aiFailedToMove p name
            Just mv -> pure mv
        Network gs -> recv gs

      -- Send the move to the other player, if needed.
      case configMoveSource config (otherPlayer p) of
        Network gs -> send gs mv
        _ -> pure ()

      pure mv

    -- Handle final state update (game over).
    final :: Outcome -> st -> IO ()
    final outcome st = do
      state st
      mode $ Finished outcome Shutdown
      takeMVar mvShutdown

    -- Handle illegal move.
    illegal :: Player -> mv -> IO ()
    illegal p _ = case configMoveSource config p of
      Human -> pure ()
      AI name _ -> do
        mode $ AIIllegalMove p name
        aiIllegalMove p name
      Network _ -> do
        mode $ NetworkIllegalMove p
        networkIllegalMove p

  pure $ GameUI
    { uiInitialUpdate = initial
    , uiAskMove = askMove
    , uiFinalUpdate = final
    , uiIllegalMove = illegal
    }

-- | Collect every event CodeWorld's reflex interface will give us,
-- and present a single stream of CodeWorld 'CW.Event's.
getCodeWorldEvent :: ReflexCodeWorld t m => m (Event t CW.Event)
getCodeWorldEvent = do
  eKeyPress <- getKeyPress
  eKeyRelease <- getKeyRelease
  eTextEntry <- getTextEntry
  dPointerPosition <- getPointerPosition
  dPointerDown <- isPointerDown
  eTimePassing <- getTimePassing

  let
    bPointerPosition = current dPointerPosition
    ePointerDown = updated dPointerDown
    ePointerClick = bPointerPosition <@ filter id ePointerDown
    ePointerRelease = bPointerPosition <@ filter not ePointerDown

  pure $ leftmost
    [ CW.KeyPress <$> eKeyPress
    , CW.KeyRelease <$> eKeyRelease
    , CW.PointerPress <$> ePointerClick
    , CW.PointerRelease <$> ePointerRelease
    , CW.PointerMovement <$> updated dPointerPosition
    , CW.TextEntry <$> eTextEntry
    , CW.TimePassing <$> eTimePassing
    ]

-- | Given a function with an on-complete callback, use an 'MVar' to
-- make it synchronous.
makeSynchronous :: (a -> IO () -> IO ()) -> a -> IO ()
makeSynchronous fire a = do
  mvDone <- newEmptyMVar
  fire a (putMVar mvDone ())
  takeMVar mvDone
