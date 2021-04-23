{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Dragons.Ataxx.Codeworld
Description : CodeWorld interface for the Ataxx game
Author      : Dhairya Patel
License     : AllRightsReserved
-}
module Dragons.Ataxx.CodeWorld where

import           Ataxx
import           CodeWorld
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Dragons.Ataxx ()
import           Dragons.Game
import           Dragons.Game.UI.CodeWorld as UI

-- | Our UI-specific state.
data UIModel
  = Idle
    -- ^ Waiting for something interesting to happen (AI move, network
    -- move, etc.).
  | SelectPiece
    -- ^ Player needs to choose a piece to move.
  | CompleteMove Location
    -- ^ Player needs to choose a square to move to.
  deriving (Eq, Show)

-- | Most events CodeWorld sends to us are uninteresting, so we parse
-- down into this structure.
data SimpleEvent
  = Esc
  | ClickLocation Location
  deriving (Eq, Show)

codeWorldUI :: GameConfig GameState Move -> IO (GameUI GameState Move)
codeWorldUI config = UI.codeWorldUI config $ CodeWorldUI
  { cwInitialModel = Idle
  , cwView = view
  , cwUpdate = update
  }

-- | Render the whole scene as a 'Picture'.
view :: UIMode Move -> GameState -> UIModel -> Picture
view mode st model = pictures
  [ drawModeText mode
  , drawModelText model
  , drawScore st
  , drawHighlights model st
  , drawLastMove st mode
  , drawBoard st
  ]

-- | Describe the 'UIMode', which is what the framework is currently
-- doing, or what it is asking of the user.
drawModeText :: UIMode Move -> Picture
drawModeText mode = translated (-5) 7 . scaled 0.5 0.5 . lettering $ case mode of
  Started -> "Initialising"
  AwaitingMove p _ _ -> pieceName p <> " to move"
  AIThinking p name -> pieceName p <> " (" <> T.pack name <> ") is thinking"
  AIFailedToMove p name -> pieceName p <> " (" <> T.pack name <> ") failed to move"
  AIIllegalMove p name -> pieceName p <> " (" <> T.pack name <> ") made an illegal move"
  NetworkIllegalMove p -> pieceName p <> " (network) made an illegal move"
  Finished o _ -> "Game over. " <> case o of
    Winner p -> pieceName p <> " wins!"
    Draw -> "It's a draw!"

-- | Additional labels from the 'UIModel', which tracks exactly what
-- we're asking of the player as he or she builds up a move.
drawModelText :: UIModel -> Picture
drawModelText model = translated (-5) 6 . scaled 0.5 0.5 $ case model of
  Idle -> blank
  SelectPiece -> lettering "Click a piece to move"
  CompleteMove _ -> lettering "Click a point to move this piece into (ESC to cancel)"

-- | Show current scores (number of pieces controlled by each player).
drawScore :: GameState -> Picture
drawScore st
  = translated 5 7 (scaled 0.5 0.5 (lettering p1))
  & translated 5 6 (scaled 0.5 0.5 (lettering p2))
  where
    p1 = "White: " <> T.pack (show p1count)
    p2 = "Black: " <> T.pack (show p2count)
    (p1count, p2count) = countPieces st

-- | Draw highlights (which pieces are selectable).
drawHighlights :: UIModel -> GameState -> Picture
drawHighlights model st = case model of
  Idle -> blank
  SelectPiece -> centreGrid st $ foldMap highlight playerPieceLocs
  CompleteMove loc -> centreGrid st . foldMap highlight $ destinationLocs loc

  where
    highlight :: Location -> Picture
    highlight (Location x y) = translated (fromIntegral x) (-(fromIntegral y)) $
      colored red (thickCircle 0.1 0.5)

    playerPieceLocs :: [Location]
    playerPieceLocs = nub . map moveFrom $ legalMoves st

    destinationLocs :: Location -> [Location]
    destinationLocs loc = nub . mapMaybe fromHere $ legalMoves st
      where
        fromHere :: Move -> Maybe Location
        fromHere mv
          | moveFrom mv == loc = Just $ moveTo mv
          | otherwise = Nothing

-- | Draw the previous move, if there is one.
drawLastMove :: GameState -> UIMode Move -> Picture
drawLastMove st mode = case mode of
  AwaitingMove _ (Just (_, Move (Location fx fy) (Location tx ty))) _ ->
    centreGrid st $ pictures
      [ translated fx' (-fy') redCircle
      , translated tx' (-ty') (colored red (solidCircle 0.25))
      , colored red (thickPolyline 0.1 [(fx', -fy'), (tx', -ty')])
      ]
    where
      redCircle = pictures
        [ colored red $ thickCircle 0.4 0.05
        , colored white $ solidCircle 0.25
        ]

      (fx', fy') = (fromIntegral fx, fromIntegral fy)
      (tx', ty') = (fromIntegral tx, fromIntegral ty)
  _ -> blank

-- | Draw the board and all its pieces to the centre of the screen.
drawBoard :: GameState -> Picture
drawBoard st = centreGrid st $
  pictures (zipWith draw (concat (board st)) (allLocations st)) & grid st
  where
    draw :: Square -> Location -> Picture
    draw sq (Location x y)
      = translated (fromIntegral x) (-(fromIntegral y)) (drawSquare sq)

-- | Draw a board square in a square 1.0 units each side.
drawSquare :: Square -> Picture
drawSquare sq = case sq of
  Empty -> blank
  Block -> colored (dark grey) (solidRectangle 0.75 0.75)
  Piece p -> case p of
    Player1 -> coloured black (circle 0.5) & coloured white (solidCircle 0.5)
    Player2 -> coloured black (solidCircle 0.5)

-- | Labels for pieces that match how we draw them.
pieceName :: Player -> Text
pieceName = player "White" "Black"

-- | The grid that makes up the board.
grid :: GameState -> Picture
grid st
  = coloured grey
  . translated (-0.5) 0.5
  $ pictures (map polyline (concat segments))
  where
    segments =
      [ map ((\y -> [(0, -y), (w', -y)]) . fromIntegral) [0..h]
      , map ((\x -> [(x, 0), (x, -h')]) . fromIntegral) [0..w]
      ]

    (w, h) = bounds st
    (w', h') = (fromIntegral w, fromIntegral h)

-- | Translate the grid into the centre of the screen.
centreGrid :: GameState -> Picture -> Picture
centreGrid st = translated (-w'/2) (h'/2)
  where
    (w, h) = bounds st
    (w', h') = (fromIntegral w, fromIntegral h)

-- | This is like the update function given to CodeWorld's
-- 'activityOf' function, but we take additional arguments for the
-- game state and 'UIMode', and return additional information to the
-- framework when we have a completed move.
update
  :: UIMode Move
  -> GameState
  -> UIModel
  -> Event
  -> (UIModel, Maybe (UIResponse Move))
update mode st model ev = case mode of
  Started -> idle
  Finished _ quit -> (Idle, Just quit)
  AIThinking _ _ -> idle
  AIFailedToMove _ _ -> idle
  AIIllegalMove _ _ -> idle
  NetworkIllegalMove _ -> idle
  AwaitingMove _ _ respond -> case model of
    Idle -> (SelectPiece, Nothing)
    SelectPiece -> withSimpleEvent $ \sev -> case sev of
      ClickLocation loc -> case find ok (legalMoves st) of
        Nothing -> (model, Nothing)
        Just _ -> (CompleteMove loc, Nothing)
        where ok mv = loc == moveFrom mv
      _ -> ignore
    CompleteMove from -> withSimpleEvent $ \sev -> case sev of
      Esc -> (SelectPiece, Nothing)
      ClickLocation to
        | Move from to `elem` legalMoves st ->
          (Idle, Just . respond $ Move from to)
        | otherwise -> (SelectPiece, Nothing)

  where
    -- Parse CodeWorld event into SimpleEvent if possible.
    simpleEvent :: Maybe SimpleEvent
    simpleEvent = case ev of
      KeyPress "Esc" -> Just Esc
      PointerPress p -> ClickLocation <$> gridPoint st p
      _ -> Nothing

    -- If the current event parses to a SimpleEvent, feed it to the
    -- function. Otherwise do nothing.
    withSimpleEvent
      :: (SimpleEvent -> (UIModel, Maybe (UIResponse Move)))
      -> (UIModel, Maybe (UIResponse Move))
    withSimpleEvent f = maybe ignore f simpleEvent

    idle = (Idle, Nothing)
    ignore = (model, Nothing)

-- | Convert a 'Point' in screen space into a 'Location' on the game
-- board, if that makes sense.
gridPoint :: GameState -> Point -> Maybe Location
gridPoint st (px, py) = find onPoint (allLocations st)
  where
    onPoint (Location x y) = dx2 + dy2 < 0.5 * 0.5
      where
        x', y' :: Double
        (x', y') = ( fromIntegral x - fromIntegral w / 2
                   , fromIntegral (-y) + fromIntegral h / 2
                   )
        (dx, dy) = (px - x', py - y')
        (dx2, dy2) = (dx * dx, dy * dy)

        (w, h) = bounds st
