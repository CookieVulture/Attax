{-|
Module      : AI
Description : AIs for Ataxx
Copyright   : Dhairya Patel
License     : AllRightsReserved
-}
module AI where

import Ataxx

-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int-> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up.


-- | Hr is heuristic. It takes a game state and return the analysis of board for
-- ai to work on.
type Hr = GameState -> Int

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("stuartBloom", NoLookahead stuartBloom),
        ("greedy", NoLookahead howardWolowitz),
        ("default", WithLookahead rajKoothrappali)
      ]


-- | Rose tree for the generations for lookahead moveset.
data Rose a = Rose a [Rose a]
  deriving (Eq, Show)

-- | AlphaBeta generates tree for Alpha Beta purning. Here, the different 
-- options are Cut, Null and Root. Cut means ignore the branch, Null means 
-- there is no value associated with this node and Root represent root of 
-- subtree.
data AlphaBeta = AB (Int, Int) | Cut | Null | Root Int
  deriving (Eq, Show)

type ABTree = Rose (Move, GameState, AlphaBeta)

-- rajKoothrappali returns best move with Alpha-Beta purning after exploring
-- 3 to 4 lookahead. If game is over it returns "The Big Bang Theory" as error.
rajKoothrappali :: GameState -> Int -> Move
rajKoothrappali state maxStage = case turn state of
  GameOver _ -> error "The Big Bang Theory"
  Turn Player1 -> bestMove state heuristic maxStage
  Turn Player2 -> bestMove state heuristic2 maxStage


-- gameTree initialise Alpha Beta purning tree for given gamestate, heuristic 
-- and lookAhead depth by selecting first possible move as parent node and then
-- creating root with negative and positive alpha beta value. moveGame function
-- take a gamestate and return all possible moves zipped with their respective
-- gamestates and it also filter out and remove same moves. 
gameTree :: GameState -> Hr -> Int -> ABTree
gameTree state heuri maxStage
  = treeRoot (treeLeave maxStage heuri maxStage (howardWolowitz state, state))
  where 
    treeRoot :: ABTree -> ABTree
    treeRoot (Rose (mov, state', _) sub)
      = Rose (mov, state', AB (-2000,2000)) sub
    treeLeave :: Int -> Hr -> Int -> (Move, GameState) -> ABTree
    treeLeave maxStage' heuri' numberOfEdges (move, state'')
     | maxStage' == 0 = Rose (move, state'', Root (heuri' state'')) []
     | maxStage' >  0 = Rose (move, state'', Null)
      (map (treeLeave (maxStage' - 1) heuri' numberOfEdges) (moveGame state''))
     | otherwise  = error "Big bang!!!"
    moveGame :: GameState -> [(Move, GameState)]
    moveGame state''' = (zip (mov) (gameS (state''')))
      where 
        mov :: [Move]
        mov = legalMoves state'''

        gameS :: GameState -> [GameState]
        gameS stat = map mayBe (applyMove' stat <$> (legalMoves stat))

        applyMove' :: GameState -> Move -> Maybe GameState
        applyMove' currentState moveNo = applyMove moveNo currentState
          


-- | bestMove chooses move by Pruning with creating alpha beta tree for given 
-- GameState , heurisitc and maximum depth to look into.
bestMove :: GameState -> Hr -> Int -> Move
bestMove state heuri maxStage
  = pickBest (pruningProcess 0 (gameTree state heuri maxStage))
  where
    pickBest :: ABTree -> Move
    pickBest (Rose (move, _, _) _) = move

-- | The main function that handles searching and pruning of the Alpha-beta 
-- rose tree
pruningProcess :: Int -> ABTree -> ABTree
pruningProcess numberOfEdges roseTree@(Rose leaf@(mov, state, AB (a,b)) sub) 
  = case sub of
  -- Phase-1 
  -- In Phase-1 the child nodes are assigned with alpha-beta value for pruning
    (Rose (mov', state', Null) sub') : elems 
      -> pruningProcess numberOfEdges
          (Rose leaf (pruningProcess (numberOfEdges+1)
            (Rose (mov',state',AB(a,b)) sub') : elems))
    
  -- Phase-2
  -- | When Sub Tree is empty, use Alpha Beta logic to decide what should be 
  --  the value of a and b with different cases and by comparing it with
  --  depth of the tree.
    (Rose (mov',_, Root heuri) []) : xs
     | numberOfEdges==0 -> case heuri >= b && heuri<=a of 
        True -> pruningProcess numberOfEdges (Rose leaf xs)
        False -> pruningProcess numberOfEdges 
          (Rose (mov', state, AB (heuri, b)) xs)

     | a < heuri && odd numberOfEdges -> case b<heuri of
        True -> pruningProcess numberOfEdges (Rose leaf xs)
        False -> pruningProcess numberOfEdges  
          (Rose (mov, state, AB (a, heuri)) xs)

     | heuri < b && even numberOfEdges -> case a>heuri of
        True -> pruningProcess numberOfEdges (Rose leaf xs)
        False -> pruningProcess numberOfEdges  
          (Rose (mov, state, AB (heuri, b)) xs)

     | otherwise -> Rose (mov, state, Cut) []

    -- Phase-3
    -- For all other cases, check the value of Alpha-Beta and compare it with 
    -- each case and replace according to depth of tree. Odd side represent 
    -- opposite player.
    (Rose (mov', _, AB (a', b')) _) : xs -> case a' < b of
      True
       | numberOfEdges == 0 -> case a < b' of
          False -> pruningProcess numberOfEdges (Rose leaf xs)
          _  -> pruningProcess numberOfEdges (Rose (mov', state, AB (b', b)) xs)
       | odd numberOfEdges
            -> pruningProcess numberOfEdges (Rose (mov, state, AB (a, a')) xs)
       | even numberOfEdges
            -> pruningProcess numberOfEdges (Rose (mov, state, AB (b', b)) xs)
      _ -> pruningProcess numberOfEdges (Rose leaf xs)
        
    -- If Branch is cut then ignore that branch.
    (Rose (_, _, Cut) _) : xs
      -> pruningProcess numberOfEdges (Rose (mov, state, AB (a, b)) xs)

    -- For all the other cases, keep the current tree.
    _ -> roseTree
pruningProcess _ roseTree = roseTree

-- End of Tree AI --

-- Start of Greedy AI --

-- howardWolowitz chooses best move by applying all the possible moves to 
-- present game state and then evaluating them to find one with highest amount
-- of difference between player and opponent. 
howardWolowitz :: GameState -> Move
howardWolowitz st = case turn st of
  GameOver _ -> error"Game over!!!!!!!"
  Turn Player1 ->  (getBest (moveFunc) (returnBst st (moveFunc)))
  Turn Player2 ->  (getBest (moveFunc) (returnBst2 st (moveFunc)))
  where moveFunc = legalMoves st

-- Player 1 heuristic.
-- This heuristic consider difference between player's pieces and opponent's 
-- pieces.
heuristic :: Hr
heuristic st = suB (countPieces st)
  where suB :: (Int,Int) -> Int
        suB s = fst(s) - snd(s)


-- The returnBst consider gameState and list of Moves to apply heuristic on
-- each moves and return list of Integer which represent heuristic of each 
-- move.
returnBst :: GameState -> [Move] -> [Int]
returnBst st moves = case moves of
  [x]-> [helper x st]
  [] -> error"No legal Moves!"
  x:xs -> helper x st:returnBst st xs

-- Helper function apply move on gameState and then return heuristic value
-- of that gameState.
helper :: Move -> GameState -> Int
helper mov sta = heuristic(mayBe(applyMove mov sta))

-- Player 2
-- This heuristic consider difference between player's pieces and opponent's 
-- pieces as we are at disadvantage when player start as Player-2. This 
-- heuristic was chosen after trial and error with heuristic of Player-1 as 
-- this returns better result.
heuristic2 :: Hr
heuristic2 st = suB (countPieces st)
  where suB :: (Int,Int) -> Int
        suB s = snd(s) - fst(s)

-- The returnBst2 consider gameState and list of Moves to apply heuristic on
-- each moves and return list of Integer which represent heuristic of each 
-- move.
returnBst2 :: GameState -> [Move] -> [Int]
returnBst2 st moves = case moves of
  [x]-> [helper2 x st]
  [] -> error"No legal Moves!"
  x:xs -> helper2 x st:returnBst2 st xs

-- Helper function apply move on gameState and then return heuristic value
-- of that gameState.
helper2 :: Move -> GameState -> Int
helper2 num sta = heuristic2(mayBe(applyMove num sta))

-- Return GameState for given Maybe GameState.
mayBe :: Maybe GameState -> GameState
mayBe st = case st of
  Just x -> x
  Nothing -> error"Game over!!!!!"

-- getBest compares list of Int and find the index of highest Int and return 
-- that move. Here, move represent all possible moves for given gameState and 
-- player and ls represent heuristic values of move. 
getBest :: [Move] -> [Int] -> Move
getBest move ls = move!!maxNum ls
  where maxNum :: [Int] -> Int 
        maxNum nums = head $ filter ((== maximum nums) . (nums !!)) [0..]

-- End of AI --

-- Start of AI --

-- | A very simple AI, which picks the first move returned by the
-- 'legalMoves' function. AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
stuartBloom :: GameState -> Move
stuartBloom st = head (legalMoves st)