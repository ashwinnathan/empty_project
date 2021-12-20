module Main where
import Control.Monad.State as S
import Lib

-- Continues playing and updating the game state until an end game condition is reached

playGame :: StateT Game IO ()
playGame = do
  game <- get
  -- Checks for checkmate/stalemate
  case checkEnd game of
    Just (Win W) -> liftIO $ putStrLn "White Wins!"
    Just (Win B) -> liftIO $ putStrLn "Black Wins!"
    Just Tie -> liftIO $ putStrLn "Stalemate"
    Nothing -> do
      move <- liftIO getLine
      -- Undo logic
      if move == "UNDO" then put $ head (history game) else
        let inputMove = parseMove move
        -- Parsing logic
          in case inputMove of
              Nothing -> liftIO $ putStrLn "Could not parse move"
              Just validMove ->
                if case getBoardPiece (board game) (start validMove) of
                  Nothing -> True
                  Just pn -> case pn of
                    (Piece wb pc) -> wb == current game
                  then
                    -- En passant case logic
                    if case (getBoardPiece (board game) (start validMove), getBoardPiece (board game) (end validMove)) of
                      (Just (Piece W Pawn), Nothing) -> case (start validMove, end validMove) of
                        ((a, b), (c, d)) -> a == 3 && c == 2 && ((b-d) == 1 || (b-d) == -1) 
                          && getBoardPiece (board game) (a, d) == Just (Piece B Pawn) && getBoardPiece (board (head (history game))) (a-2, d) == Just (Piece B Pawn)
                      (Just (Piece B Pawn), Nothing) -> case (start validMove, end validMove) of
                        ((a, b), (c, d)) -> a == 4 && c == 5 && ((b-d) == 1 || (b-d) == -1) 
                          && getBoardPiece (board game) (a, d) == Just (Piece W Pawn) && getBoardPiece (board (head (history game))) (a+2, d) == Just (Piece W Pawn)
                      _ -> False
                      then
                        put $ makeMoveEP game validMove
                      else (
                        -- Castling case logic
                        if validMove == Move (0, 4) (0, 2) || validMove == Move (0, 4) (0, 6) || validMove == Move (7, 4) (7, 2) || validMove == Move (7, 4) (7, 6)
                          then (if validCastle validMove game then do
                            put $ makeMoveSamePlayer game validMove
                            game2 <- get
                            case validMove of
                              Move (0, 4) (0, 2) -> put $ makeMove game2 (Move (0, 0) (0, 3))
                              Move (0, 4) (0, 6) -> put $ makeMove game2 (Move (0, 7) (0, 5))
                              Move (7, 4) (7, 2) -> put $ makeMove game2 (Move (7, 0) (7, 3))
                              Move (7, 4) (7, 6) -> put $ makeMove game2 (Move (7, 7) (7, 5))
                              _ -> liftIO $ putStrLn "Should not occur"
                              else liftIO $ putStrLn "Invalid castling") else
                            -- Logic for if a player checks the other player, or puts themselves in check
                          ( if valid (board game) validMove
                              then
                                if not (isInCheck (makeMoveSamePlayer game validMove ))
                                  then do
                                    if isInCheck (makeMove game validMove )
                                      then liftIO $ putStrLn "In check"
                                      else pure ()
                                      --Checks for valid moves
                                    let updateGame = makeMove game validMove
                                    put updateGame
                                  else liftIO $ putStrLn "You are in check"
                              else liftIO $ putStrLn "Invalid move"
                          ))
                  else liftIO $ putStrLn "Wrong player's move"
      newGame <- get
      liftIO $ displayBoard (board newGame)
      playGame

-- Main loop
main :: IO ()
main = do
  displayBoard initialBoard
  evalStateT playGame initialGame