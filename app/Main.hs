module Main where

import Control.Monad.State as S
import Lib

-- Retrieve user move input and progress game state until end

playGame :: StateT Game IO ()
playGame = do
  game <- get
  case checkEnd game of
    Just (Win W) -> liftIO $ putStrLn "White Wins!"
    Just (Win B) -> liftIO $ putStrLn "Black Wins!"
    Just Tie -> liftIO $ putStrLn "Stalemate"
    Nothing -> do
      liftIO $ putStrLn "Input next move"
      move <- liftIO getLine
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
                        if not (isInCheck (makeMoveSamePlayer game validMove ))
                          then
                            ( if isInCheck (makeMove game validMove )
                                then
                                  ( do
                                      put $ makeMove game validMove
                                      liftIO $ putStrLn "Check!"
                                  )
                                else put $ makeMove game validMove
                            )
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