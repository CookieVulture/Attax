{-|
Module      : AITests
Description : Tests for your AI functions
Author      : Dhairya Patel
License     : AllRightsReserved
-}

module TestPattern where

import Ataxx

-- All the gamesBoard with appropriate gameStates requiered for testing

-- Board-1
-- Testcases Name : test1Return1, test2Return2, test1Helper2, test1Helper2,
-- x
startBoard :: Board
startBoard =
  [ [ Piece Player1, Empty, Empty ]
  , [ Empty, Empty, Empty ]
  , [ Empty, Empty, Piece Player2 ]
  ]

startGameState :: GameState
startGameState  = State (Turn Player2) (3, 3) startBoard []

-- Board-2
startBoardPlayer1 :: Board
startBoardPlayer1 =
  [ [ Piece Player1, Piece Player1, Empty ]
  , [ Empty, Empty, Empty ]
  , [ Empty, Empty, Piece Player2 ]
  ]

startGameStateP1 :: GameState
startGameStateP1  = State (Turn Player2) (3, 3) startBoardPlayer1 [startBoard]

-- Board-3
-- Testcases Name : test1Helper, test2Helper
startBoardPlayer2 :: Board
startBoardPlayer2 =
  [ [ Piece Player1, Empty, Empty ]
  , [ Empty, Empty, Empty ]
  , [ Empty, Piece Player2, Piece Player2 ]
  ]

startGameStateP2 :: GameState
startGameStateP2  = State (Turn Player1) (3, 3) startBoardPlayer2 
 [startBoard,
  startBoardPlayer2]

-- Board-4
almostFilledBoard :: Board
almostFilledBoard = 
  [ [ Piece Player1, Piece Player1, Piece Player2 ]
  , [ Piece Player1, Empty, Piece Player2 ]
  , [ Piece Player1, Empty, Piece Player2 ]
  ]

-- Different list of moves for test cases

-- List(Moves) - 1
-- Testcases Names : testGetBest
listOfMoves :: [Move]
listOfMoves = [Move (Location 0 0) (Location 0 1), 
               Move (Location 0 0) (Location 1 0)]

-- List(Moves) - 2
-- Testcases Names : test1Return2
listOfMoves' :: [Move]
listOfMoves' = [(Move (Location 2 2) (Location 2 0)),
                (Move (Location 2 2) (Location 2 1))
               ]

-- List(Moves) - 3
-- Testcases Names : test2Return2
listOfMoves'' :: [Move]
listOfMoves'' = [(Move (Location 2 2) (Location 2 0))]


-- Different list of Heuristic for appropriate test cases

-- List(Int) - 1
-- Testcases Names : testGetBest
listOfInt :: [Int]
listOfInt = [0,5]