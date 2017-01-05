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
type WhitePieces = [Piece]
type BlackPieces = [Piece]
type BoardSquare = (Coordinate, Maybe Piece)
--
-- Data constructors
data Board = Board {whitePieces::[Piece], blackPieces::[Piece], boardSquares::[BoardSquare]}
--  deriving Show
instance Show Board where
  show b@(Board _ _ sqrs) = rowSplit ++ "\n" ++ concatMap showSquare sqrs
    where
      showSquare ((x, y), p)
        | x == 8 = squareDisplay p ++ "\n" ++ rowSplit ++ "\n"
        | x == 1 = "| " ++ squareDisplay p
        | otherwise = squareDisplay p
      squareDisplay p 
        | isJust p = " " ++ (show $ fromJust p) ++ " |"
        | otherwise = "    |"
      rowSplit = replicate 42 '-'

data Piece = Piece {pieceName::PieceName, pieceColor::PieceColor, pieceCoord::Coordinate}
instance Show Piece where
  show p = show (pieceColor p) ++ show (pieceName p)
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

initialBoard = Board wPieces bPieces initializeSquares 
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


-- Board dimensions are from (1, 1) to (8, 8)
isInBoard :: Coordinate -> Bool
isInBoard (x, y) = x > 0  && x < 9 && y > 0 && y < 9

hasPieceAt :: Board -> Coordinate -> Bool
hasPieceAt board coord
  | not $ isInBoard coord = False
  | isNothing piece = False
  | otherwise = True
  where
    piece = fromJust $ lookup coord (boardSquares board)

--pieceAt :: Board -> Coordinate -> Maybe Piece

-- Pieces functions
--
isWhite (Piece _ White _) = True
isWhite _ = False

isBlack (Piece _ Black _) = True
isBlack _ = False
-- Pawns
--
{-
-- It has to be at initial position AND have free space
pawnInitialMove :: Board -> Piece -> Board
pawnInitialMove b@(Board wPieces bPieces sqrs) p@(Piece _ color (x, y))
  | isWhite p && y == 2 && 

genericMoves (Piece Knight _ (x, y)) = map (\(a, b) -> (x + a, y + b)) knightMoves
  where
    knightMoves = rotate ((-1), 2) ++ rotate (1, 2)
    rotate = take 4 . iterate (\(x, y) -> (-y, x))
genericMoves (Piece _ _ _) = [(1, 1)]
--blockedMove
--move :: Board -> Piece -> [Board]
-}
