import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (isJust)
import Test.HUnit hiding (State)
import Test.QuickCheck
-- import State (State)
-- import qualified State as S

--module Lib
--   (initialGame
--   )
-- where

data Player = W | B deriving (Eq, Show)

data Location = Loc Int Int deriving (Eq, Ord, Show)

data Piece = Piece Player PieceName deriving (Eq)

data PieceName = Pawn | Rook | Bishop | Knight | King | Queen deriving (Eq, Show)

type Board = [[Maybe Piece]]

data Game = Game {board :: Board, current :: Player} deriving (Eq, Show)

data End = Win Player | Tie deriving (Eq, Show)

data Move = Move {start :: Location, end :: Location} deriving (Eq, Show)

-- | starting board for the game
initialGame :: State Game Move
initialGame = undefined

-- | is the board still playable
checkEnd :: Board -> Maybe End
checkEnd = undefined

-- | is the move valid
valid :: Board -> Move -> Bool
valid = undefined

getBoardPiece :: Board -> Location -> Maybe Piece
getBoardPiece = undefined

-- | update game state with move
makeMove :: Move -> State Game Move
makeMove = undefined

generateMoves :: Piece -> Location -> [Location]
generateMoves = undefined



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
