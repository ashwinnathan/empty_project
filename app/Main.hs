module Main where

import Control.Monad.State as S
import Lib

-- Retrieve user move input and progress game state until end
playGame :: StateT Game IO ()
playGame = do
  liftIO $ putStrLn "Input next move"
  move <- liftIO getLine
  game <- get
  let inputMove = parseMove move
   in case inputMove of
        Nothing -> liftIO $ putStrLn "Could not parse move"
        Just validMove ->
          if case getBoardPiece (board game) (start validMove) of
            Nothing -> True
            Just pn -> case pn of
              (Piece wb pn) -> wb == current game
            then
              ( if valid (board game) validMove
                  then
                    if not (isInCheck (makeMoveSamePlayer validMove game))
                      then do
                        if isInCheck (makeMove validMove game)
                          then liftIO $ putStrLn "In check"
                          else pure ()
                        let updateGame = makeMove validMove game
                        put $ updateGame {history = Just game}
                        liftIO $ putStrLn "Do you want to undo (Y/N)?"
                        res <- liftIO getLine
                        case res of
                          "Y" -> put game
                          _ -> pure ()
                      else liftIO $ putStrLn "You are in check"
                  else liftIO $ putStrLn "Invalid move"
              )
            else liftIO $ putStrLn "Wrong player's move"
  newGame <- get
  liftIO $ displayBoard (board newGame)
  playGame

main :: IO ()
main = do
  displayBoard initialBoard
  evalStateT playGame initialGame