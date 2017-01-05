-- The goal of this module is to validate the understanding of 
--  alpha beta pruning/minmax algorithms.
-- Once this is fully understood, the data structure for the chess board will 
--  be defined
import Data.Maybe

data NodeType = MinNode | MaxNode
  deriving Show

type AlphaBeta = (Maybe Int, Maybe Int) 

data RoseTree a = Node a [RoseTree a] | Leaf a
  deriving Show

-- Util
--
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

isNode (Node _ _) = True
isNode _ = False

isMaxNode MaxNode = True
isMaxNode _ = False
-- For testing purposes, only multiples of 3 (validation sets are formatted like this)
--    Validation set: http://inst.eecs.berkeley.edu/~cs61b/fa14/ta-materials/apps/ab_tree_practice/
--    So you can have leaves with 3 data and a multiple 3 number of leaves (3, 9, 27)
testTree = generateTree [8, 2, (-14), 5, 2, 9, 2, (-20), (-17)]

generateTree :: [Int] -> RoseTree Int
generateTree leaves 
  | isLastLayer = Node 0 $ map Leaf leaves
  | otherwise = Node 0 $ map generateTree leavesChunk
  where
    leavesChunk = splitEvery ((length leaves) `div` 3) leaves
    isLastLayer = length (head leavesChunk) == 1

computeRating (Leaf n) = n

-- Alpha beta pruning tests starts here.
--  The goal of the algorithm will be to return the the best node (on the first layer)
--  Thus, the result of the alpha beta prunning should be a subtree which will then become
--   the maximal minimal return. The depth of the tree shouldn't matter.
--  THIS ASSUMES THAT THE RATING CAN BE COMPUTED. The rating algorithm will be replaced in time.

{-
checkLeaves (alpha, beta) MinNode leaves= (alpha, beta)
checkLeaves (alpha, beta) MaxNode leaves
  | isNothing alpha && isNothing beta = (Just $ maximum $ map computeRating leaves, beta)
  | otherwise =  
  where
    betaValue = fromJust beta
    scanLeaves 
scanWithAlphaBeta (alpha, beta) nodeType tree
  | isNode tree && isMaxNode 
-}
