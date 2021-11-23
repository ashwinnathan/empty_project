import Data.Maybe (isJust)
import qualified Data.Map as M
import Test.HUnit
import Test.QuickCheck
import Control.Monad.State
-- import State (State)
-- import qualified State as S

-- module Lib
--   (initialGame
--   )
-- where

data Player = W | B deriving (Eq, Show)

data Location = Loc Int Int deriving (Eq, Ord, Show)

data Piece = Piece Player PieceName deriving (Eq)

data PieceName = Pawn | Rook | Bishop | Knight | King | Queen deriving (Eq, Show)

type Board = [[Maybe Piece]]

data Game = Game { board :: Board , current :: Player } deriving (Eq, Show)

data End = Win Player | Tie deriving (Eq, Show)

data Move = Move {start:: Location, end:: Location} deriving (Eq, Show)

-- | starting board for the game
initialGame :: State Game Move
initialGame = undefined

-- | is the board still playable
checkEnd :: Board -> Maybe End
checkEnd = undefined

-- | is the move valid
valid :: Board -> Move -> Bool
valid = undefined

-- | update game state with move
makeMove :: Move -> State Game Move
makeMove = undefined

class Monad m => Interface m where
        -- ask the current player for their next move
    getMove       :: Game -> m Location
    -- send a message to players
    message       :: String -> m ()
    showBoard       :: Game -> m ()
    
 -- | make moves until someone wins
playGame :: Interface m => State Game Move -> m ()
playGame game = do
     x <- S.get
    message showBoard (board game)
    case checkEnd $ board game of
     Just (Win p)  -> message $ "Player " ++ show p ++ " wins!"
     Just Tie      -> message $ "It's a Tie!"
     Nothing       -> do
       playerMessage (current game) $ "It's your turn" 
       move <- getMove game
       case makeMove game move of
         Just game' -> playGame game'
         Nothing    -> error "BUG: move is invalid!"
         
instance Interface IO where
    getMove       = undefined
    playerMessage = undefined
    message       = undefined

instance Arbitrary Game where
    arbitrary = Game <$> arbitrary <*> arbitrary
instance Arbitrary Player where
    arbitrary = elements [W, B]
instance Arbitrary Pieces where
    arbitrary = elements pieces



















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


