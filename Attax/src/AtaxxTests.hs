{-|
Module      : AtaxxTests
Description : Tests for the Ataxx game
Author      : Dhairya Patel
License     : AllRightsReserved
-}
module AtaxxTests where

import Ataxx
import Data.Aeson
import Dragons.Ataxx ()
import Dragons.Ataxx.Text
import Testing

ataxxTests :: Test
ataxxTests = TestGroup "Ataxx"
  [ initialStateTests
  , legalMovesTests
  , countPiecesTests
  , applyMoveTests
  , jsonTests
  , moveParsingTests
  ]

initialStateTests :: Test
initialStateTests = TestGroup "initialState"
  [ Test "correct height"
      (assertEqual (length (board (initialState (7,8)))) 8)
  , Test "correct widths"
      (assertEqual (boardWidths (initialState (7, 8))) (replicate 8 7))
  ]

legalMovesTests :: Test
legalMovesTests = TestGroup "legalMoves"
  [ Test "on intialState"
      (assertEqual (legalMoves (initialState (7, 7)))
        -- 7 moves from each corner piece, duplicating moves first.
        -- (The blocks on the board obstruct one jumping move from
        -- each piece.)
        -- Top-left, duplicating
        [ Move (Location 0 0) (Location 0 1)
        , Move (Location 0 0) (Location 1 0)
        , Move (Location 0 0) (Location 1 1)

        -- Bottom-right, duplicating
        , Move (Location 6 6) (Location 5 5)
        , Move (Location 6 6) (Location 5 6)
        , Move (Location 6 6) (Location 6 5)

        -- Top-left, jumping
        , Move (Location 0 0) (Location 0 2)
        , Move (Location 0 0) (Location 1 2)
        , Move (Location 0 0) (Location 2 0)
        , Move (Location 0 0) (Location 2 1)

        -- Bottom-right, jumping
        , Move (Location 6 6) (Location 4 5)
        , Move (Location 6 6) (Location 4 6)
        , Move (Location 6 6) (Location 5 4)
        , Move (Location 6 6) (Location 6 4)
        ])
  ]

countPiecesTests :: Test
countPiecesTests = TestGroup "countPieces"
  [ Test "on small custom board"
      (assertEqual (countPieces (State (Turn Player1) (3, 3) smallBoard []))
        (2, 3))
  ]

applyMoveTests :: Test
applyMoveTests = TestGroup "applyMove"
  [ Test "capturing on small custom board"
      (assertEqual
        (applyMove
          (Move (Location 1 2) (Location 0 2))
          (State (Turn Player1) (3, 3) smallBoard []))
        (Just (State (Turn Player2) (3, 3) smallBoard' [])))
  ]

jsonTests :: Test
jsonTests = TestGroup "JSON encode/decode"
  [ Test "simple encode/decode of Move"
      (assertEqual (decode (encode mv)) (Just mv))
  ]
  where mv = Move (Location 3 2) (Location 4 5)

moveParsingTests :: Test
moveParsingTests = TestGroup "move parsing/unparsing"
  [ Test "reading roundtrip"
      (assertEqual (renderMove <$> parseMove st "A6-A5") (Just "A6-A5"))
  , Test "printing roundtrip"
      (assertEqual
        (parseMove st (renderMove (Move (Location 6 0) (Location 5 0))))
        (Just (Move (Location 6 0) (Location 5 0))))
  ]
  where st = initialState (7, 7)

boardWidths :: GameState -> [Int]
boardWidths = map length . board

smallBoard :: Board
smallBoard =
  [ [ Piece Player1, Block,         Empty         ]
  , [ Piece Player2, Piece Player2, Block         ]
  , [ Empty,         Piece Player1, Piece Player2 ]
  ]

smallBoard' :: Board
smallBoard' =
  [ [ Piece Player1, Block,         Empty         ]
  , [ Piece Player1, Piece Player1, Block         ]
  , [ Piece Player1, Piece Player1, Piece Player2 ]
  ]
