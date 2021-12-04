import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe (isJust)
import Test.HUnit hiding (State)
import Test.QuickCheck

data Player = W | B

type Position = (Int, Int)

data Piece = Piece Player PieceName

showPiece :: Piece -> Char
showPiece (Piece player p) = case player of
  W -> toLower (showPieceName p)
  B -> showPieceName p

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

readPieceName :: Char -> PieceName
readPieceName 'P' = Pawn
readPieceName 'R' = Rook
readPieceName 'B' = Bishop
readPieceName 'N' = Knight
readPieceName 'K' = King
readPieceName 'Q' = Queen
readPieceName _ = error "No such piece"

initialBoard :: Board
initialBoard =
  [map ((Just . Piece B) . readPieceName) "RNBKQBNR", replicate 8 (Just (Piece B Pawn))]
    ++ replicate 4 (replicate 8 Nothing)
    ++ [replicate 8 (Just (Piece W Pawn)), map ((Just . Piece W) . readPieceName) "RNBKQBNR"]

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
initialGame :: State Game Move
initialGame = undefined

-- | is the board still playable
checkEnd :: Board -> Maybe End
checkEnd = undefined

-- | is the move valid
valid :: Board -> Move -> Bool
valid = undefined

getBoardPiece :: Board -> Position -> Piece
getBoardPiece = undefined

-- | update game state with move
makeMove :: Move -> State Game ()
makeMove = undefined

generateMoves :: Piece -> Position -> [Position]
generateMoves = undefined

class Monad m => Interface m where
  -- ask the current player for their next move
  getMove :: Game -> m Position

  -- send a message to players
  message :: String -> m ()
  showBoard :: Game -> m ()

-- Retrieve user move input and progress game state until end
-- playGame :: Interface m => State Game Move -> m ()
-- playGame game = do
--      x <- S.get
--     message showBoard (board game)
--     case checkEnd $ board game of
--      Just (Win p)  -> message $ "Player " ++ show p ++ " wins!"
--      Just Tie      -> message $ "It's a Tie!"
--      Nothing       -> do
--        playerMessage (current game) $ "It's your turn"
--        move <- getMove game
--        case makeMove game move of
--          Just game' -> playGame game'
--          Nothing    -> error "BUG: move is invalid!"

instance Interface IO where
  getMove = undefined
  showBoard = undefined
  message = undefined

-- --Ex 1
-- scrapper :: String -> IO (Maybe [Game])
-- scrapper url = scrapeURL url comments
-- where
--     games :: Scraper String [Game]
--     games = chroot ("div" @: ["id" @= "events"])

--     game :: Scraper String Game
--     game = do
--         gameId <- -- getGameId
--         Teams <- -- getTeams
--         return Game {gameId = gameId, Teams = Teams}

-- dayData :: [Game] ->
