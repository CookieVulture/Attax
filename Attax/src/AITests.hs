{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2020 Dhairya Patel
License     : AllRightsReserved
-}
module AITests where

import AI
import Ataxx
import Testing
import TestPattern

-- Important Note for Tutor 
--
-- test?_ = ? is natural number starting  from 1. For example, test1, test2 and
-- so on. It is done where same function is checked for different cases/.

aiTests :: Test
aiTests = TestGroup "AI"
  [testGetBest,
   test1Helper2,
   test2Helper2,
   test1Return2,
   test2Return2,
   test1Helper,
   test2Helper,
   testGreedy,
   testAlpha]
   --testRandom

-- test?GetBest

-- | getBest function should return best move from list for given list of
-- heuristic value of those moves by choosing best heuristic value. Here, 
-- move-2 should be chose as its heuristic value is 5 compared to 1st move with
-- 0 heuristic value. Here 0 and 5 are randomly chosen.
testGetBest :: Test
testGetBest = 
 Test "Test if best move is chose" (assertEqual (getBest listOfMoves listOfInt) 
  ((Move (Location 0 0) (Location 1 0))::Move)) 

-- end of test?GetBest


-- | test?Helper2 checks if helper2 is working according to its condition 
-- of returns right heuristic value. Helper2 test also prove that heuristic2 is
-- also working. So, we don't need extra test for it. 

-- Logic of test1 - The current board has 1 player1 piece and 1 player2 piece
-- and if it is turn of player-2 and it copies the piece it should return 1 as
-- heuristic of player2 is Black-White which will be 1.

-- Logic of test2 - The current board has 1 player1 piece and 1 player2 piece
-- and if it is turn of player-2 and it jump the piece it should return 0 as
-- heuristic of player2 is Black-White which will be 0.
test1Helper2 :: Test
test1Helper2 = 
 Test "Test if Helper2 returns right heuristic value for copy" 
  (assertEqual (helper2 (Move (Location 2 2) (Location 2 1)) startGameState) 
   (1::Int))

test2Helper2 :: Test
test2Helper2 = 
 Test "Test if Helper2 returns right heuristic value for jump" 
  (assertEqual (helper2 (Move (Location 2 2) (Location 2 0)) startGameState) 
   (0::Int))

-- End of test?Helper2

-- | Test?Return2 - It checks if returnBst2 is working and returns list of
--  all the heuristic values for given moves and gameBoard.

-- Logic of test1 - Test1 should return 0 and 1 in list as they are heuristic
--  value for given gameState and moves list.

-- Logic of test2 - Test2 check weather the function work on singular element
--  or not

test1Return2 :: Test
test1Return2 = 
 Test "Test if returnBst2 returns true heuristic for list of moves"
  (assertEqual (returnBst2 startGameState listOfMoves' ) 
   ([0,1]::[Int]))

test2Return2 :: Test
test2Return2 = 
 Test "Test if returnBst2 returns true heuristic for list of single move"
  (assertEqual (returnBst2 startGameState listOfMoves'' ) 
   ([0]::[Int]))

-- | Test?Heuristic2 - Heuristic2 doesn't need seperate test as test?Helper2 
--  also checks Heuristic2 function. 

-- End of Test?Heuristic2


-- | test?Helper checks if helper is working according to its condition 
-- of returns right heuristic value. Helper test also prove that heuristic is
-- also working. So, we don't need extra test for it. 

-- Logic of test1 - The current board has 1 player1 piece and 2 player2 piece
-- and if it is turn of player-1 and it copies the piece it should return 0 as
-- heuristic of player1 is White-Black which will be 0.

-- Logic of test2 - The current board has 1 player1 piece and 2 player2 piece
-- and if it is turn of player-1 and it jump the piece it should return 0 as
-- heuristic of player1 is White-Black which will be -1.
test1Helper :: Test
test1Helper = 
 Test "Test if Helper returns right heuristic value for copy" 
  (assertEqual (helper (Move (Location 0 0) (Location 1 0)) startGameStateP2) 
   (0::Int))

test2Helper :: Test
test2Helper = 
 Test "Test if Helper returns right heuristic value for jump" 
  (assertEqual (helper (Move (Location 0 0) (Location 2 0)) startGameStateP2) 
   (-1::Int))

-- End of test?Helper2


-- | testGreedy checks if HowardWolowitz or Greedy is working fine and returning
--  best move or not.

-- Here test should return Move (Location 0 0) (Location 1 1) as it is more
-- logical move compared to others in current gameState. Greedy never looks in 
-- future to decide best solution. It takes best move from given possibilities.
testGreedy :: Test
testGreedy =
  Test "If Greedy returns right move"
   (assertEqual (howardWolowitz startGameStateP2)
    ((Move (Location 0 0) (Location 1 1))::Move) )

-- Here test of Alpha-Beta returns (Location 2 2) (Location 1 1) as it is one
-- move win for player-2. All the possible conditions are explored with Alpha
--  Beta purning.

testAlpha :: Test
testAlpha =
  Test "If Alpha-Beta returns right move"
   (assertEqual (rajKoothrappali startGameState 2)
    ((Move (Location 2 2) (Location 1 1))::Move) )