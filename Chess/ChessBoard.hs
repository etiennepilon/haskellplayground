module ChessBoard where

import Data.Maybe

-- Board standards
--     Y
--     |
--     |
--     |
--     |------ X
--    (1, 1)
-- 
-- Types 
type Coordinate = (Int, Int)
type Move = (Int, Int)
type BoardSquare = (Coordinate, Maybe Piece)
--
-- Data constructors
data Board = Board {boardSquares::[BoardSquare]}
--  deriving Show
instance Show Board where
  show b@(Board sqrs) = rowSplit ++ "\n" ++ concatMap showSquare sqrs ++ bottomIndexes
    where
      showSquare ((x, y), p)
        | x == 8 = squareDisplay p ++ "\n" ++ rowSplit ++ "\n"
        | x == 1 = (show y) ++ " |" ++ squareDisplay p
        | otherwise = squareDisplay p
      squareDisplay p 
        | isJust p = " " ++ (show $ fromJust p) ++ " |"
        | otherwise = "    |"
      rowSplit = replicate 42 '-'
      bottomIndexes = "  " ++ concatMap (\x -> "  " ++ show x ++ "  ") [1..8]

data Piece = Piece {pieceName::PieceName, pieceColor::PieceColor, pieceCoord::Coordinate}
instance Show Piece where
  show p = show (pieceColor p) ++ show (pieceName p)
instance Eq Piece where
  (==) p1 p2 = pieceCoord p1 == pieceCoord p2
instance Ord Piece where
  compare p1 p2 = compare (pieceCoord p1) (pieceCoord p2)

data PieceName = Pawn | Rook | Knight | Bishop | Queen | King
instance Show PieceName where
  show Pawn = "P"
  show Rook = "R"
  show Knight = "K"
  show Bishop = "B"
  show Queen = "Q"
  show King = "†" -- alt + t
data PieceColor = Black | White
instance Show PieceColor where
  show White = "w"
  show Black = "b"

initialBoard = Board initializeSquares 
  where
    initializeSquares = map squareAt [(x, y) | y <- [8, 7, 6, 5, 4, 3, 2, 1], x <- [1..8]] -- List comprehension built like this to avoid sorting in instance Show
    squareAt coord 
      | (length piece) == 0 = (coord, Nothing)
      | otherwise = (coord, Just $ head piece)
      where
        piece = filter (\p -> (pieceCoord p) == coord) (bPieces ++ wPieces)
    bPieces = blackBishop ++ [blackKing] ++ blackKnight ++ blackPawns ++ [blackQueen] ++ blackRooks
    blackBishop = map (Piece Bishop Black) [(x, y) | x <- [3, 6], y <- [8]]
    blackKing = Piece King Black (5, 8)
    blackKnight = map (Piece Knight Black) [(x, y) | x <- [2, 7], y <- [8]]
    blackPawns = map (Piece Pawn Black) [(x, y) | x <- [1..8], y <- [7]]
    blackQueen =  Piece Queen Black (4, 8)
    blackRooks = map (Piece Rook Black) [(x, y) | x <- [1, 8], y <- [8]]
    wPieces = whiteBishop ++ [whiteKing] ++ whiteKnight ++ whitePawns ++ [whiteQueen] ++ whiteRooks
    whiteBishop = map (Piece Bishop White) [(x, y) | x <- [3, 6], y <- [1]]
    whiteKing = Piece King White (5, 1)
    whiteKnight = map (Piece Knight White) [(x, y) | x <- [2, 7], y <- [1]]
    whitePawns = map (Piece Pawn White) [(x, y) | x <- [1..8], y <- [2]]
    whiteQueen =  Piece Queen White (4, 1)
    whiteRooks = map (Piece Rook White) [(x, y) | x <- [1, 8], y <- [1]]
-- Board functions
--

whitePieces :: Board -> [Piece]
whitePieces board = concatMap (extract . snd) (boardSquares board)
  where
    extract Nothing = []
    extract (Just (Piece _ Black _)) = []
    extract p = [fromJust p]

blackPieces :: Board -> [Piece]
blackPieces board = concatMap (extract . snd) (boardSquares board)
  where
    extract Nothing = []
    extract (Just (Piece _ White _)) = []
    extract p = [fromJust p]

-- Board dimensions are from (1, 1) to (8, 8)
isInBoard :: Coordinate -> Bool
isInBoard (x, y) = x > 0  && x < 9 && y > 0 && y < 9

hasPieceAt :: Board -> Coordinate -> Bool
hasPieceAt board coord
  | isNothing piece = False
  | otherwise = True
  where
    piece = pieceAt board coord

hasWhitePieceAt :: Board -> Coordinate -> Bool
hasWhitePieceAt board coord
  | isNothing piece = False
  | isBlack $ fromJust piece = False
  | otherwise = True
  where
    piece = pieceAt board coord

hasBlackPieceAt :: Board -> Coordinate -> Bool
hasBlackPieceAt board coord
  | isNothing piece = False
  | isBlack $ fromJust piece = False
  | otherwise = True
  where
    piece = pieceAt board coord

pieceAt :: Board -> Coordinate -> Maybe Piece
pieceAt board coord 
  | not $ isInBoard coord = Nothing
  | otherwise = fromJust $ lookup coord (boardSquares board)

-- Pieces functions
--
isWhite (Piece _ White _) = True
isWhite _ = False

isBlack (Piece _ Black _) = True
isBlack _ = False

movedPiece :: Piece -> Coordinate -> Piece
movedPiece (Piece n c (x, y)) (a, b) = Piece n c (x + a, y + b)
-- Pawns
--

{-    
pawnForward :: Board -> Piece -> [Board]
pawnForward board piece
  | isWhite piece && not $ hasPieceAt (pieceCoordWithMove (0, 1)) = 
  

--whiteMoves :: Board -> [Board]
--whiteMoves board = concatMap (nextMoves board) (whitePieces board)
nextMoves :: Board -> Piece -> [Board]
nextMoves board p@(Piece Pawn _ _) = pawnMoves board p
nextMoves board p@(Piece Rook _ _) = rookMoves board p
nextMoves board p@(Piece Bishop _ _) = bishopMoves board p
nextMoves board p@(Piece Knight _ _) = knightMoves board p
nextMoves board p@(Piece King _ _) = kingMoves board p
nextMoves board p@(Piece Queen _ _) = queenMoves board p
-}
{-
genericMoves (Piece Knight _ (x, y)) = map (\(a, b) -> (x + a, y + b)) knightMoves
  where
    knightMoves = rotate ((-1), 2) ++ rotate (1, 2)
    rotate = take 4 . iterate (\(x, y) -> (-y, x))
genericMoves (Piece _ _ _) = [(1, 1)]
--blockedMove
--move :: Board -> Piece -> [Board]
-}
