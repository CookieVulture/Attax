{-|
Module      : Ataxx
Description : Implementation of the Ataxx board game
Copyright   : (c) 2020 Dhairya Patel
License     : AllRightsReserved
-}
module Ataxx where

import Control.DeepSeq
import Data.Function ((&))
import Data.List
import GHC.Generics (Generic)

-- | The game state contains whose turn it is, the board size, the
-- current board, and a history of previous boards to detect loops. If
-- the game returns to a previous board setup, we assume that we're
-- stuck in a loop and declare the game a draw. We only start tracking
-- history when players make non-duplicating moves, to limit the total
-- amount of data carried around.
data GameState = State Turn (Int, Int) Board [Board] deriving (Eq, Show)

turn :: GameState -> Turn
turn (State t _ _ _) = t

adjustTurn :: (Turn -> Turn) -> GameState -> GameState
adjustTurn f (State t bnd brd h) = State (f t) bnd brd h

setTurn :: Turn -> GameState -> GameState
setTurn t = adjustTurn (\_ -> t)

bounds :: GameState -> (Int, Int)
bounds (State _ b _ _) = b

board :: GameState -> Board
board (State _ _ b _) = b

setBoard :: Board -> GameState -> GameState
setBoard b (State t bnd _ h) = State t bnd b h

-- | The history of previous boards that we check for loops. Note that
-- this is not a full history going back to the start of the game; we
-- clear history whenever a piece duplicates. (As pieces never leave
-- the board, duplicating means the board configuration /must/ be new.)
history :: GameState -> [Board]
history (State _ _ _ h) = h

adjustHistory :: ([Board] -> [Board]) -> GameState -> GameState
adjustHistory f (State t bnd brd h) = State t bnd brd (f h)

clearHistory :: GameState -> GameState
clearHistory = adjustHistory (\_ -> [])

initialState :: (Int, Int) -> GameState
initialState (w, h) = addBlocks $ State (Turn Player1) (w, h)
  ([ [Piece Player1] ++ replicate (w - 2) Empty ++ [Piece Player2] ]
   ++ replicate (h - 2) (replicate w Empty) ++
   [ [Piece Player2] ++ replicate (w - 2) Empty ++ [Piece Player1] ])
  []

  where
    addBlocks = compose
      (  map (\y -> setSquare (Location w2 y) Block) [0, h - 1]
      ++ map (\x -> setSquare (Location x (h2 - 1)) Block) [2..middleBarEnd]
      ++ map (\x -> setSquare (Location x (h2 + 1)) Block) [2..middleBarEnd]
      )

    h2 = h `div` 2
    w2 = w `div` 2
    middleBarEnd = w - 3

-- | A player is either player 1 or 2.
data Player = Player1 | Player2 deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer p = case p of
  Player1 -> Player2
  Player2 -> Player1

data Turn = Turn Player | GameOver Outcome deriving (Eq, Show)
data Outcome = Winner Player | Draw deriving (Eq, Show)

-- | A location on the board is an X then a Y coordinate.
data Location = Location Int Int deriving (Eq, Generic, NFData, Show)

-- | Is the 'Location' on our board?
onBoard :: GameState -> Location -> Bool
onBoard st (Location x y) = x >= 0 && x < w && y >= 0 && y < h
  where (w, h) = bounds st

-- | Chebyshev distance, aka chessboard distance between two
-- 'Location's. How many steps to get from one location to another.
chebyshev :: Location -> Location -> Int
chebyshev (Location x1 y1) (Location x2 y2) =
  abs (x2 - x1) `max` abs (y2 - y1)

allLocations :: GameState -> [Location]
allLocations st =
  [ Location x y | y <- [0..(h - 1)], x <- [0..(w - 1)] ]
  where (w, h) = bounds st

-- | A square on the board.
data Square
  = Piece Player -- ^ Occupied by a player's piece.
  | Block -- ^ Occupied by a block. Can be jumped over.
  | Empty -- ^ Empty space.
  deriving (Eq, Show)

-- | A 'Board' always consists of 'gameHeight' rows, each of length
-- 'gameWidth'. The top-left corner is @'Location' 0 0@.
type Board = [[Square]]

get :: GameState -> Location -> Maybe Square
get st (Location x y)
  | onBoard st (Location x y) = Just (board st !! y !! x)
  | otherwise = Nothing

adjustSquareAt :: Location -> (Square -> Square) -> GameState -> GameState
adjustSquareAt loc@(Location x y) f st = case get st loc of
  Nothing -> st
  Just s -> st'
    where
      st' = setBoard b' st

      b' = beforeRows ++ [changedRow] ++ afterRows
      changedRow = beforeSquares ++ [f s] ++ afterSquares

      (beforeSquares, _:afterSquares) = splitAt x changingRow
      (beforeRows, changingRow:afterRows) = splitAt y (board st)

setSquare :: Location -> Square -> GameState -> GameState
setSquare loc s = adjustSquareAt loc (const s)

clearSquare :: Location -> GameState -> GameState
clearSquare loc = setSquare loc Empty

-- | Move your piece from the first location to the second, which must
-- be empty. A valid move must be either adjacent or only one step
-- away. (i.e., Chebyshev distance is between 1 and 2 inclusive.)
data Move = Move Location Location
  deriving (Eq, Generic, NFData, Show)

moveFrom :: Move -> Location
moveFrom (Move from _) = from

moveTo :: Move -> Location
moveTo (Move _ to) = to

data MoveType = Duplicate | Jump deriving (Eq, Show)

classifyMove :: Move -> Maybe MoveType
classifyMove (Move from to)
  | d == 1 = Just Duplicate
  | d == 2 = Just Jump
  | otherwise = Nothing
  where d = chebyshev from to

applyMove :: Move -> GameState -> Maybe GameState
applyMove (Move from to) st = case turn st of
  -- Game must not yet be over
  Turn player -> case get st from of
    -- "From" location must name one of our pieces.
    Just (Piece p)
      | p == player -> case get st to of
          -- "To" location must be vacant.
          Just Empty -> case classifyMove (Move from to) of
            Just mvType -> Just (nextTurn (applyCaptures p (movePiece p mvType st)))
            Nothing -> Nothing
          _ -> Nothing
    _ -> Nothing
  GameOver _ -> Nothing

  where
    movePiece :: Player -> MoveType -> GameState -> GameState
    movePiece p mvType = case mvType of
      Duplicate -> clearHistory . setSquare to (Piece p)
      Jump -> compose
        [ adjustHistory (board st:)
        , setSquare to (Piece p)
        , clearSquare from
        ]

    -- Claim all occupied squares around your location.
    applyCaptures :: Player -> GameState -> GameState
    applyCaptures p = compose
      (map (\loc -> adjustSquareAt loc (capture p)) capturePoints)

    capture :: Player -> Square -> Square
    capture p sq = case sq of
      Piece _ -> Piece p
      _ -> sq

    capturePoints :: [Location]
    capturePoints = case to of
      Location x y -> [ Location (x + x') (y + y')
                      | x' <- [-1..1]
                      , y' <- [-1..1]
                      , x' /= 0 || y' /= 0
                      ]


-- | Set the next turn in the game state:
--
-- 1. If the current board exists in our history, it's a draw.
-- 2. If the other player can make a move, it's his or her turn.
-- 3. If the other player cannot, the game is over; tally the scores.
nextTurn :: GameState -> GameState
nextTurn st
  | board st `elem` history st = setTurn (GameOver Draw) st
  | otherwise = case legalMoves st' of
      [] -> setTurn (GameOver outcome) st
      _ -> st'
  where
    st' = adjustTurn next st

    (p1count, p2count) = countPieces st

    outcome = case compare p1count p2count of
        LT -> Winner Player2
        GT -> Winner Player1
        EQ -> Draw

    next :: Turn -> Turn
    next trn = case trn of
      Turn p -> Turn (otherPlayer p)
      GameOver _ -> trn

countPieces :: GameState -> (Int, Int)
countPieces st = foldl' count (0, 0) (concat (board st))
  where
    count :: (Int, Int) -> Square -> (Int, Int)
    count (p1,p2) sq = case sq of
      Piece Player1 -> (p1 + 1, p2)
      Piece Player2 -> (p1, p2 + 1)
      _  -> (p1, p2)

-- | Return the list of legal moves for the current player.
legalMoves :: GameState -> [Move]
legalMoves st = case turn st of
  GameOver _ -> []
  Turn p -> zipWith moves (concat (board st)) (allLocations st)
    & concat
    -- list duplicating moves first, for firstLegalMove's sake
    & sortOn moveDistance
    where
      moveDistance :: Move -> Int
      moveDistance (Move from to) = chebyshev from to

      moves :: Square -> Location -> [Move]
      moves sq loc = case sq of
        Piece piece | piece == p -> map (Move loc) (movePoints loc)
        _ -> []

      movePoints ::  Location -> [Location]
      movePoints (Location x y) =
        [ Location (x + x') (y + y')
        | x' <- [-2..2]
        , y' <- [-2..2]
        , (x' /= 0 || y' /= 0)
          && get st (Location (x + x') (y + y')) == Just Empty
        ]

-- | Given a list of functions, compose them all together.
compose :: [a -> a] -> a -> a
compose = foldr (.) id
