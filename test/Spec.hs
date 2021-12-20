import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = putStrLn "Test suite not yet implemented"

makeBoard :: [[Maybe Piece]] -> Board
makeBoard = undefined

testValidMove :: Test 
testValidMove = TestList [
    "Knight" ~: not (valid initialBoard (0,0) (1,4))
    "Queen" ~: not (valid initialBoard (0,0) (1,4))
]

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

-- isJust (makeMove game move) where move is a random generated index and is part of the list  of generatedmoves,
--  make sure that the board is valid 

instance Arbitrary Player where
    arbitrary = elements [W, B]
instance Arbitrary PieceName where
    arbitrary = elements [Pawn, Rook, Bishop, Knight, King, Queen]
instance Arbitrary Piece where
    arbitrary = Piece <$> arbitrary <*> arbitrary
instance Arbitrary Board where
    arbitrary = undefined --sized list

x :: Board
x = [[Just $ Piece B Rook, Just $ Piece B Knight, Just $ Piece B Bishop, Nothing, Just $ Piece B King, Just $ Piece B Bishop, Just $ Piece B Knight, Just $ Piece B Rook],
    [Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn, Nothing, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn],
    [Nothing, Nothing, Nothing, Nothing, Just $ Piece B Pawn, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece W Pawn, Just $ Piece B Queen],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece W Pawn, Nothing, Nothing],
    [Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Nothing, Nothing, Just $ Piece W Pawn],
    [Just $ Piece W Rook, Just $ Piece W Knight, Just $ Piece W Bishop, Just $ Piece W Queen, Just $ Piece W King, Just $ Piece W Bishop, Just $ Piece W Knight, Just $ Piece W Rook]]

xs :: Game
xs = Game {board = x, current = W}
