import Euler.Util (isSquare)

squares :: [Integer]
squares = map (^ 2) [1 ..]

tuples :: Int -> [a] -> [[a]]
tuples 0 _ = [[]]
tuples n lst = [x : xs | (x, k) <- zip lst [0 ..], xs <- tuples (n - 1) (take k lst)]

-- If x > y > z > 0, then
-- a = x + y > b = x + z > c = y + z

main :: IO ()
main = do
  let (x, y, z) =
        head
          [ (x, y, z)
            | [a, b, c] <- tuples 3 squares,
              even (a + b - c), -- If one of them is even, all of them are!
              let x = (a + b - c) `div` 2,
              x > 0,
              let y = (a + c - b) `div` 2,
              y > 0,
              let z = (b + c - a) `div` 2,
              z > 0,
              isSquare (x - y) && isSquare (x - z) && isSquare (y - z)
          ]
  print $ x + y + z
