{-|
Module      : Dragons.Ataxx.Text
Description : Text interface for the Ataxx game
Author      : Dhairya Patel
License     : AllRightsReserved
-}
module Dragons.Ataxx.Text where

import Ataxx
import Data.Char
import Data.Maybe
import Dragons.Ataxx ()
import Dragons.Game
import Dragons.Game.UI.Text as UI

textUI :: GameConfig GameState Move -> GameUI GameState Move
textUI config = UI.textUI config $ TextUI
  { textRenderState = renderState
  , textReadMove = readMove
  }

renderState :: GameState -> String
renderState st = unlines $ catMaybes
  [ renderTurn (turn st)
  , Just $ renderBoard st
  ]

renderTurn :: Turn -> Maybe String
renderTurn t = case t of
  Turn Player1 -> Just "Player 1 (O) to move"
  Turn Player2 -> Just "Player 2 (X) to move"
  GameOver _ -> Nothing

renderBoard :: GameState -> String
renderBoard (State _ (w,_) brd _) = unlines $
  heading : zipWith (:) ['0'..'9'] ((map . map) renderSquare brd)
  where
    heading :: String
    heading = ' ' : take w ['A'..'Z']

    renderSquare :: Square -> Char
    renderSquare sq = case sq of
      Empty -> ' '
      Block -> '#'
      Piece Player1 -> 'O'
      Piece Player2 -> 'X'

-- | Ask for a move, check that it's sensible, and if it isn't, ask again.
readMove :: GameState -> Maybe (Player, Move) -> IO Move
readMove st _ = loop
  where
    loop = do
      putStrLn $ "Enter a move. Example: " ++ renderMove (head (legalMoves st))
      line <- getLine
      case parseMove st line of
        Nothing -> errLoop "Malformed move, try again."
        Just mv
          | get st (moveFrom mv) /= Just (Piece p) ->
              errLoop "You don't own a piece at the move's origin."
          | get st (moveTo mv) /= Just Empty ->
              errLoop "Destination square not empty"
          | chebyshev (moveFrom mv) (moveTo mv) > 2 ->
              errLoop "That move is too far."
          | otherwise -> pure mv

    errLoop s = putStrLn s *> loop

    Turn p = turn st

-- | Parse a 'String' that should describe a 'Move', if it makes sense
-- for where we are in the current game.
parseMove :: GameState -> String -> Maybe Move
parseMove st s = case map toUpper s of
  [ff, fr, '-', tf, tr] ->
    case (fromFile ff, fromRank fr, fromFile tf, fromRank tr) of
      (Just fx, Just fy, Just tx, Just ty) ->
        Just (Move (Location fx fy) (Location tx ty))
      _ -> Nothing

    where
      fromFile r
        | x >= 0 && x < w = Just x
        | otherwise = Nothing
        where
          x = ord r - ord 'A'

      fromRank f
        | y >= 0 && y < h = Just y
        | otherwise = Nothing
        where
          y = ord f - ord '0'

      (w,h) = bounds st
  _ -> Nothing

renderMove :: Move -> String
renderMove (Move (Location fx fy) (Location tx ty))
  = fs ++ "-" ++ ts
  where
    fs = [ff, fr]
    ts = [tf, tr]

    ff = toFile fx
    fr = toRank fy
    tf = toFile tx
    tr = toRank ty

    toRank n = ['0'..'9'] !! n
    toFile n = ['A'..'Z'] !! n
