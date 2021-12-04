module Main where

import Lib

main :: IO ()
main = playGame initialGame

class Monad m => Interface m where
  -- ask the current player for their next move
  getMove :: Game -> m Location
  -- send a message to players
  message :: String -> m ()
  showBoard :: Game -> m ()

-- Retrieve user move input and progress game state until end
playGame :: Interface m => State Game Move -> m ()
playGame = undefined
-- playGame gameState = do
--     game <- S.get
--     message showBoard (board game)
--     case checkEnd $ board game of
--      Just (Win p)  -> message $ "Player " ++ show p ++ " wins!"
--      Just Tie      -> message $ "It's a Tie!"
--      Nothing       -> do
--        playerMessage (current game) $ "It's your turn"
--        move <- getMove game
--        S.set move
--        case makeMove game move of
--          Just game' -> playGame game'
--          Nothing    -> error "BUG: move is invalid!"

instance Interface IO where
  getMove = undefined
  message = undefined