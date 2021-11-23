import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = putStrLn "Test suite not yet implemented"


makeBoard :: [[Maybe Piece]] -> Board
makeBoard = undefined

testCheckEnd :: Test
testCheckEnd = TestList [
     "Win for B"           ~: checkEnd winBoard ~?= Just (Win B)
   , "Initial is playable" ~: checkEnd (board initialGame) ~?= Nothing
   , "Tie game"            ~: checkEnd tieBoard ~?= Just Tie
   ] where
       winBoard = -- makeWinning board for black
       tieBoard = -- make stalemate board

-- a quickcheck property about validity
prop_validMove :: Game -> Location -> Bool
prop_validMove game move =
    isJust (makeMove game move) == valid (board game) move
