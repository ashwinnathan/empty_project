import Control.Monad.State as S
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Numeric (readInt)
import Test.HUnit hiding (State)
import Test.QuickCheck

data Player = W | B

type Position = (Int, Int)

data Piece = Piece Player PieceName

instance Show Piece where
  show (Piece player p) = case player of
    B -> [toLower (showPieceName p)]
    W -> [showPieceName p]

showPiece :: Piece -> Char
showPiece (Piece player p) = case player of
  B -> toLower (showPieceName p)
  W -> showPieceName p

data PieceName = Pawn | Rook | Bishop | Knight | King | Queen deriving (Eq)

showPieceName :: PieceName -> Char
showPieceName Pawn = 'P'
showPieceName Rook = 'R'
showPieceName Bishop = 'B'
showPieceName Knight = 'N'
showPieceName King = 'K'
showPieceName Queen = 'Q'

type Board = [[Maybe Piece]]

data Game = Game {board :: Board, current :: Player}

data End = Win Player | Tie

data Move = Move {start :: Position, end :: Position}

instance Show Move where
  show mv =
    let (x, y) = start mv
     in let (x2, y2) = end mv
         in show x ++ "," ++ show y ++ " " ++ show x2 ++ "," ++ show y2

readPieceName :: Char -> PieceName
readPieceName 'P' = Pawn
readPieceName 'R' = Rook
readPieceName 'B' = Bishop
readPieceName 'N' = Knight
readPieceName 'K' = King
readPieceName 'Q' = Queen
readPieceName _ = error "No such piece"

letterToNumberMap :: [(Char, Char)]
letterToNumberMap = zip ['A' .. 'H'] ['1' .. '8']

initialBoard :: Board
initialBoard =
  [map ((Just . Piece B) . readPieceName) "RNBQKBNR", replicate 8 (Just (Piece B Pawn))]
    ++ replicate 4 (replicate 8 Nothing)
    ++ [replicate 8 (Just (Piece W Pawn)), map ((Just . Piece W) . readPieceName) "RNBQKBNR"]

stringifyBoard :: Board -> [String]
stringifyBoard = map (map (maybe '.' showPiece))

displayBoard :: Board -> IO ()
displayBoard b = do
  putStrLn $ "  " ++ intersperse ' ' ['A' .. 'H']
  putStrLn $ "  " ++ replicate 15 '-'
  mapM_
    (\s -> let spacedString = intersperse ' ' s ++ "\n" in putStrLn spacedString)
    (zipWith (:) (reverse ['1' .. '8']) (stringifyBoard b))

-- | starting board for the game
initialGame :: Game
initialGame = Game {board = initialBoard, current = W}

-- | is the board still playable
checkEnd :: Board -> Maybe End
checkEnd = undefined

-- | is the move valid
valid :: Board -> Move -> Bool
valid b (Move oldP@(oldX, oldY) newP@(newX, newY)) =
  let currPiece = getBoardPiece b oldP
   in case currPiece of
        Nothing -> False
        Just piece -> inBounds newP && newP `elem` generateMoves piece oldP

inBounds :: Position -> Bool
inBounds (x, y) = x >= 0 && y >= 0 && x <= 8 && y <= 8

getBoardPiece :: Board -> Position -> Maybe Piece
getBoardPiece b (x, y) = (b !! x) !! y

parseMove :: String -> Maybe Move
parseMove str = case words str of
  [[oldX, oldY], [newX, newY]] -> do
    x1 <- lookup oldX letterToNumberMap
    x2 <- lookup newX letterToNumberMap
    return $ Move (8 - (read [oldY] :: Int), (read [x1] :: Int) - 1) (8 - (read [newY] :: Int), (read [x2] :: Int) - 1)
  _ -> Nothing

-- | update game state with move
makeMove :: Move -> Game -> Game
makeMove mv st = case getBoardPiece (board st) (start mv) of
  Nothing -> error "Invalid move"
  Just piece -> case current st of
    W -> Game (updateBoard (board st) piece mv) B
    B -> Game (updateBoard (board st) piece mv) W

updateBoard :: Board -> Piece -> Move -> Board
updateBoard board p mv = moveHelper (moveHelper board Nothing (start mv)) (Just p) (end mv)
  where
    moveHelper :: Board -> Maybe Piece -> Position -> Board
    moveHelper m x (r, c) = take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m

generateMoves :: Piece -> Position -> [Position]
generateMoves (Piece player p) pos@(x, y) = case p of
  -- Pawn can only move in y-axis UNLESS it captures opposing piece
  Pawn -> case player of
    W -> [(newX, newY) | newX <- [x - 1 .. x + 1], newY <- [y .. y + 1], newY /= y]
    B -> [(newX, newY) | newX <- [x - 1 .. x + 1], newY <- [y - 1 .. y], newY /= y]
  -- Knight has specified movement so generate list of relative move vectors and add to existing coordinates
  Knight -> [(x + newX, y + newY) | (newX, newY) <- [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]]
  -- King can only move in the surrounding square
  King -> [(newX, newY) | newX <- [x - 1 .. x + 1], newY <- [y - 1 .. y + 1]]
  -- Rook can move both vertical and horizontal, but only on one axis
  Rook -> xyVectors pos
  -- Bishops can only move diagonally
  Bishop -> diagVectors pos
  --Queen can move on all three axes
  Queen -> xyVectors pos ++ diagVectors pos

xyVectors :: Position -> [Position]
xyVectors (x, y) = [(newX, y) | newX <- [1 .. 8]] ++ [(x, newY) | newY <- [1 .. 8]]

diagVectors :: Position -> [Position]
diagVectors (x, y) = [(x + newX, y + newY) | newX <- [(-8) .. 8], newY :: Int <- [(-8) .. 8], newX == newY]

-- Retrieve user move input and progress game state until end
playGame :: StateT Game IO ()
playGame = do
  liftIO $ putStrLn "Input next move"
  move <- liftIO getLine
  game <- get
  let inputMove = parseMove move
   in case inputMove of
        Nothing -> liftIO $ putStrLn "Could not parse move"
        Just validMove -> put $ makeMove validMove game
  newGame <- get
  liftIO $ displayBoard (board newGame)
  playGame

main :: IO ()
main = evalStateT playGame initialGame