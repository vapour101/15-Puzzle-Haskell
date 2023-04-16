import Data.Map ((!))
import Data.Map qualified as Map
import Data.Tuple (swap)

type Board = Map.Map Int Int

data Move = U | D | L | R deriving (Enum)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

coord :: Int -> (Int, Int)
coord x = (x `mod` 4, x `div` 4)

index :: (Int, Int) -> Int
index (x, y) = x + 4 * y

dist :: Int -> Int -> Int
dist val pos = manhattan (coord val) (coord pos)

heuristic :: Board -> Int
heuristic = Map.foldrWithKey (\x y -> (+) $ dist' x y) 0
  where
    dist' 15 _ = 0
    dist' val pos = dist val pos

coordAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
coordAdd (x, y) (u, v) = (x + u, y + v)

makeMove :: Move -> Board -> Maybe Board
makeMove move board | isInvalid = Nothing
  where
    isInvalid = invalidCoord $ moveDelta (reverseMove move) `coordAdd` coord (board ! 15)
    invalidCoord (x, y) = outOfBounds x || outOfBounds y
    outOfBounds n = n < 0 || n > 3
makeMove move board = Just newBoard
  where
    newBoard = swapPos fromVal 15
    swapPos x y = Map.insert x (board ! y) $ Map.insert y (board ! x) board
    fromVal = index $ coord (board ! 15) `coordAdd` moveDelta (reverseMove move)

valueAt :: Board -> Int -> Int
valueAt board pos = fst . head . Map.toList . Map.filter (== pos) $ board

reverseMove :: Move -> Move
reverseMove U = D
reverseMove D = U
reverseMove L = R
reverseMove R = L

moveDelta :: Move -> (Int, Int)
moveDelta U = (0, -1)
moveDelta D = (0, 1)
moveDelta L = (-1, 0)
moveDelta R = (1, 0)

