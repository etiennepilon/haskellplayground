import Data.Maybe

type Rating = Int

data NodeType = MinNode | MaxNode
  deriving Show
data MinMaxTree a = Node NodeType a [MinMaxTree a] | Leaf NodeType a
  deriving Show

type Alpha = Maybe Int
type Beta = Maybe Int
type AlphaBeta = (Alpha, Beta)

data Dummy = N [Dummy] | L Int
-- Temporary rating computation
computeRating :: Int -> Int
computeRating = id

-- What should it return? The branch with the hit to make. The winning node?
--alphaBetaPruning :: MinMaxTree Int ->  
-- |
-- dummy shit
minmaxLeaf :: NodeType -> Int -> MinMaxTree Int
minmaxLeaf t a = Leaf t a 

minmaxLeafLayer :: NodeType -> [Int] -> [MinMaxTree Int]
minmaxLeafLayer t as = map (minmaxLeaf t) as

swapType MinNode = MaxNode
swapType MaxNode = MinNode

-- it will generate a tree with only value at the leafs
generateTree :: NodeType -> Dummy -> MinMaxTree Int
generateTree t (N xs)= Node t 0 (map (generateTree (swapType t)) xs)
generateTree t (L x) = minmaxLeaf t x

testAlphaBeta = (Nothing, Nothing)
oneLayerTree = generateTree MinNode $ N [L (-6), L (-13), L (4)]

leafs (Node _ _ xs) = xs

dummyDepth4Tree = generateTree MaxNode $ 
                  N [
                      N [N [L (-6), L (-13), L (-13)], N [L (-15), L (-15), L (-6)], N [L (10), L (16), L (-7)]],
                      N [N [L (-7), L (-18), L (-7)], N [L (-5), L (9), L (-7)], N [L (14), L (1), L (-11)]],
                      N [N [L (-10), L (12), L (-9)], N [L (12), L (0), L (5)], N [L (-3), L (-13), L (19)]]
                    ]

-- œ: alt+q, ß: alt+s
--minNode :: AlphaBeta -> MinMaxTree Int -> AlphaBeta
-- isNothing ß && isNothing œ: This happens when the first branch is searched
minNodeBeforeLeaves (œ,ß) leaves
  | isNothing ß && isNothing œ = (œ, Just $ minimum $ map rating leaves) 
  | isJust œ = findBeta leaves (fromJust œ, 
  | otherwise = (œ,ß)
  where
    findBeta (x:xs) (alpha, beta)
      | r <= alpha = (alpha, beta) -- Pruning
      | otherwise = findBeta xs (alpha, r)
      where
        r = rating x
    rating (Leaf _ a) = computeRating a
