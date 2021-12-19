import Control.Monad.State as S
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Ord (Down (getDown))
import Numeric (readInt)
import Test.HUnit hiding (State)
import Test.QuickCheck

data Direction = V | H | D

instance Show Direction where
  show d = case d of
    V -> show 'V'
    H -> show 'H'
    D -> show 'D'

data Player = W | B deriving (Eq, Show)

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

-- | TODO: Checkmate logic I think is as follows
-- generate all possible valid move positions for a given player
-- fold over the list of moves and call isInCheck on each one, if at least one potential move returns false then we know not a checkmate
checkEnd :: Board -> Maybe End
checkEnd = undefined

-- generate all opponent valid moves and check if current player king is in the
-- new position of any valid move (meaning it can be "captured")
isInCheck :: Game -> Bool
isInCheck g =
  foldr
    ( \position acc -> case getBoardPiece (board g) position of
        Nothing -> acc
        Just (Piece player pieceName) -> pieceName == King || acc
    )
    False
    (generateAllOppMoves (board g) (current g))

generateAllOppMoves :: Board -> Player -> [Position]
generateAllOppMoves b pl =
  foldr
    ( \(x, y) acc -> case getBoardPiece b (x, y) of
        Nothing -> acc
        Just p@(Piece player pieceName) ->
          if player /= pl
            then filter (valid b . Move (x, y)) (generateMoves b p (x, y)) ++ acc
            else acc
    )
    []
    [(newX, newY) | newX <- [0 .. 7], newY <- [0 .. 7]]

-- generateAllOppMoves = foldR

-- | is the move valid
valid :: Board -> Move -> Bool
valid b m@(Move oldP@(oldX, oldY) newP@(newX, newY)) =
  let currPiece = getBoardPiece b oldP
   in case currPiece of
        Nothing -> False
        Just piece@(Piece player pieceName) ->
          inBounds newP && newP `elem` generateMoves b piece oldP && case pieceName of
            Knight -> canCaptureOrIsEmpty b m player
            Rook -> doesIntersect b m (getDirection m) && canCaptureOrIsEmpty b m player
            Queen -> doesIntersect b m (getDirection m) && canCaptureOrIsEmpty b m player
            Bishop -> doesIntersect b m (getDirection m) && canCaptureOrIsEmpty b m player
            King ->
              isNothing (getBoardPiece b newP)
                || ((oldX /= newX && newX /= newY) && canCaptureOrIsEmpty b m player)
            _ -> True

getDirection :: Move -> Direction
getDirection m@(Move oldP@(oldX, oldY) newP@(newX, newY)) = case (oldX - newX, oldY - newY) of
  (0, _) -> H
  (_, 0) -> V
  _ -> D

-- inBounds newP && newP `elem` generateMoves piece oldP

inBounds :: Position -> Bool
inBounds (x, y) = x >= 0 && y >= 0 && x <= 7 && y <= 7

getBoardPiece :: Board -> Position -> Maybe Piece
getBoardPiece b (x, y) = if inBounds (x, y) then b !! x !! y else Nothing

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

isPlayer :: Maybe Piece -> Player -> Bool
isPlayer p pl = case p of
  Nothing -> False
  Just (Piece player pieceName) -> player == pl

generateMoves :: Board -> Piece -> Position -> [Position]
generateMoves b (Piece player p) pos@(x, y) = case p of
  -- Pawn can only move in y-axis UNLESS it captures opposing piece
  Pawn -> case player of
    W ->
      ([(4, y) | x == 6])
        ++ ([(x - 1, y) | isNothing (getBoardPiece b (x - 1, y))])
        ++ ([(x - 1, newY) | newY <- [y - 1, y + 1], isPlayer (getBoardPiece b (x - 1, newY)) B])
    B ->
      ([(3, y) | x == 1])
        ++ ([(x + 1, y) | isNothing (getBoardPiece b (x + 1, y))])
        ++ ([(x + 1, newY) | newY <- [y - 1, y + 1], isPlayer (getBoardPiece b (x + 1, newY)) W])
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

canCaptureOrIsEmpty :: Board -> Move -> Player -> Bool
canCaptureOrIsEmpty b m@(Move oldP newP) p1 = case getBoardPiece b newP of
  Nothing -> True
  Just (Piece p2 pieceName) -> p1 /= p2

doesIntersect :: Board -> Move -> Direction -> Bool
doesIntersect b m@(Move oldP@(oldX, oldY) newP@(newX, newY)) d =
  case d of
    V -> foldr (\x acc -> isNothing (getBoardPiece b (x, newY)) && acc) True [min newX oldX + 1 .. max newX oldX - 1]
    H -> foldr (\y acc -> isNothing (getBoardPiece b (oldX, y)) && acc) True [min newY oldY + 1 .. max newY oldY - 1]
    D -> True

-- D ->
--   if oldX - newX == oldY - newY
--     then foldr (\(x, y) acc -> isNothing (getBoardPiece b (x, y)) && acc) True [(newX, newY) | newX <- [min newX oldX + 1 .. max newX oldX - 1], newY <- [min newY oldY + 1 .. max newY oldY - 1], newX == newY]
--     else foldr (\(x, y) acc -> isNothing (getBoardPiece b (x, y)) && acc) True [(newX, newY) | newX <- [min newX oldX + 1 .. max newX oldX - 1], newY <- [min newY oldY + 1 .. max newY oldY - 1], newX * (-1) == newY]

xyVectors :: Position -> [Position]
xyVectors (x, y) = [(newX, y) | newX <- [0 .. 7]] ++ [(x, newY) | newY <- [0 .. 7]]

diagVectors :: Position -> [Position]
diagVectors (x, y) =
  [(x + newX, y + newY) | newX <- [(-8) .. 8], newY :: Int <- [(-8) .. 8], newX == newY || newX * (-1) == newY]

-- Retrieve user move input and progress game state until end
playGame :: StateT Game IO ()
playGame = do
  liftIO $ putStrLn "Input next move"
  move <- liftIO getLine
  game <- get
  let inputMove = parseMove move
   in case inputMove of
        Nothing -> do 
          liftIO $ putStrLn "Could not parse move"
          playGame
        Just validMove -> do 
          if valid (board game) validMove then put $ makeMove validMove game else liftIO $ putStrLn "Invalid move"
          newGame <- get
          liftIO $ displayBoard (board newGame)
          let currPlayer = current newGame in 
            if isInCheck newGame 
              then liftIO $ putStrLn (show currPlayer ++ " is in check! Next move " ++ show currPlayer) 
              else liftIO $ putStrLn ("Next move " ++ show currPlayer) 
          put (newGame {current = if current game == W then B else W})
          playGame

main :: IO ()
main = evalStateT playGame initialGame