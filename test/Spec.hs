import Test.HUnit
import Test.QuickCheck
import Lib

-- Run all tests
main :: IO ()
main = do
        putStrLn "Unit tests"
        _ <- runTestTT $ TestList [testCheckMate, testStaleMate, testCheck, testCastling, testValidMove]
        putStrLn "QuickCheck"
        quickCheck prop_validMove
        quickCheck prop_inboundsMove

-- Checkmate unit tests
testCheckMate :: Test
testCheckMate = "testCheckMate" ~: TestList [
  checkEnd ys ~=? Just (Win W),
  checkEnd xs ~=? Just (Win B),
  checkEnd chmG ~=? Just (Win W)]

-- Different piece's valid moves
testValidMove :: Test 
testValidMove = "testValidMove" ~: TestList [
  valid capt (Move (2, 0) (3, 0)) ~=? True,
  valid capt2 (Move (2, 1) (3, 0)) ~=? True,
  valid noCapt (Move (2, 1) (3, 0)) ~=? False]

-- Stalemate unit tests
testStaleMate :: Test
testStaleMate = "testStaleMate" ~: TestList [
  checkEnd zs ~=? Just Tie,
  checkEnd as ~=? Just Tie]

-- Check unit tests
testCheck :: Test
testCheck = "testCheck" ~: TestList [
  isInCheck chk2 ~=? False,
  isInCheck chk ~=? True,
  isInCheck chmG ~=? True]

-- Castling unit tests
testCastling :: Test
testCastling = "testCastling" ~: TestList [
  validCastle (Move (0, 4) (0, 2)) cast1 ~=? True,
  validCastle (Move (0, 4) (0, 2)) cast2 ~=? False,
  validCastle (Move (0, 4) (0, 2)) cast3 ~=? False]

-- Arbitrary instances for Pieces
instance Arbitrary Player where
    arbitrary = elements [W, B]
instance Arbitrary PieceName where
    arbitrary = elements [Pawn, Rook, Bishop, Knight, King, Queen]
instance Arbitrary Piece where
    arbitrary = Piece <$> arbitrary <*> arbitrary

-- Tests the prop that any valid move of a piece will be a subset of the generated all possible moves
prop_validMove :: Piece -> Position -> Position -> Bool
prop_validMove pc pos1 pos2 =
  not (inBounds pos1 && valid (moveHelper emp (Just pc) pos1) (Move pos1 pos2)) || elem pos2 (generateMoves (moveHelper emp (Just pc) pos1) pc pos1)

-- Tests the prop that all moves generated inBounds (when there is only one piece on the board) will be valid moves
prop_inboundsMove :: Piece -> Position -> Bool
prop_inboundsMove pc pos = not (inBounds pos) ||
  helper (moveHelper emp (Just pc) pos) pos (generateMoves (moveHelper emp (Just pc) pos) pc pos) where
    helper a b (c:tl) = if inBounds c then valid a (Move b c) || helper a b tl else helper a b tl
    helper _ _ [] = True


-- Boards and Games used for the unit tests
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
xs = Game {board = x, current = W, history = []}

y :: Board
y = [[Nothing, Just $ Piece W Rook, Nothing, Nothing, Just $ Piece B King, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Just $ Piece W King, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

ys :: Game
ys = Game {board = y, current = B, history = []}

z :: Board
z = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B King],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Just $ Piece B Pawn, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece W Queen, Nothing],
    [Just $ Piece W Queen, Nothing, Just $ Piece W King, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

capt :: Board
capt = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B King],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Just $ Piece B Rook, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece W Queen, Nothing],
    [Just $ Piece W Queen, Nothing, Just $ Piece W King, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

capt2 :: Board
capt2 = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B King],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Just $ Piece B Pawn, Nothing, Nothing, Nothing, Nothing, Just $ Piece W Queen, Nothing],
    [Just $ Piece W Queen, Nothing, Nothing, Just $ Piece W King,  Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

noCapt :: Board
noCapt = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B King],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Just $ Piece B Pawn, Nothing, Nothing, Nothing, Nothing, Just $ Piece W Queen, Nothing],
    [Nothing, Nothing, Nothing, Just $ Piece W King, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

zs :: Game
zs = Game {board = z, current = B, history = []}

a :: Board
a = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B King],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece W Queen, Nothing],
    [Nothing, Nothing, Just $ Piece W King, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

as :: Game
as = Game {board = z, current = B, history = []}

ch :: Board
ch = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B King],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Just $ Piece W King, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Just $ Piece W Bishop, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

chm :: Board
chm = [[Just $ Piece W Queen, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B King],
    [Just $ Piece W Rook, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Just $ Piece W King, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

chmG :: Game
chmG = Game {board = chm, current = B, history = []}

chk :: Game
chk = Game {board = ch, current = B, history = []}

chk2 :: Game
chk2 = Game {board = ch, current = W, history = []}

emp :: Board
emp = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

cast :: Board
cast = [[Just $ Piece B Rook, Nothing, Nothing, Nothing, Just $ Piece B King, Just $ Piece B Bishop, Just $ Piece B Knight, Just $ Piece B Rook],
    [Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn, Nothing, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn],
    [Nothing, Nothing, Nothing, Nothing, Just $ Piece B Pawn, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B Queen],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn],
    [Just $ Piece W Rook, Just $ Piece W Knight, Just $ Piece W Bishop, Just $ Piece W Queen, Just $ Piece W King, Just $ Piece W Bishop, Just $ Piece W Knight, Just $ Piece W Rook]]

castWrong :: Board
castWrong = [[Just $ Piece B Rook, Nothing, Nothing, Just $ Piece B King, Nothing, Just $ Piece B Bishop, Just $ Piece B Knight, Just $ Piece B Rook],
    [Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn, Nothing, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn],
    [Nothing, Nothing, Nothing, Nothing, Just $ Piece B Pawn, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B Queen],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn],
    [Just $ Piece W Rook, Just $ Piece W Knight, Just $ Piece W Bishop, Just $ Piece W Queen, Just $ Piece W King, Just $ Piece W Bishop, Just $ Piece W Knight, Just $ Piece W Rook]]

castCheck :: Board
castCheck = [[Just $ Piece B Rook, Nothing, Nothing, Nothing, Just $ Piece B King, Just $ Piece B Bishop, Just $ Piece B Knight, Just $ Piece B Rook],
    [Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece W Rook, Just $ Piece B Pawn, Just $ Piece B Pawn, Just $ Piece B Pawn],
    [Nothing, Nothing, Nothing, Nothing, Just $ Piece B Pawn, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just $ Piece B Queen],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn, Just $ Piece W Pawn],
    [Nothing, Just $ Piece W Knight, Just $ Piece W Bishop, Just $ Piece W Queen, Just $ Piece W King, Just $ Piece W Bishop, Just $ Piece W Knight, Just $ Piece W Rook]]


cast1 :: Game
cast1 = Game {board = cast, current = B, history = []}

cast2 :: Game
cast2 = Game {board = cast, current = B, history = [Game {board = castWrong, current = B, history = []}]}

cast3 :: Game
cast3 = Game {board = castCheck, current = B, history = []}