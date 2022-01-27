numTriangles :: Int -> Integer -- [((Int, Int), (Int, Int))]
numTriangles n =
  -- Triangles with 90 degree angle at origin
  fromIntegral n ^ 2
    + sum
      [ m
        | -- Let (x, y) be the point where the 90 degree angle is at
          x <- [0 .. n],
          y <- [0 .. n],
          not (x == 0 && y == 0),
          -- Then the other non-zero point (a, b) must be of the form (x, y) + alpha * (y, -x) / d, where d = gcd x y
          let d = gcd x y,
          -- Since 0 \le a, b, \le n, we have upper and lower bounds for alpha
          let m =
                if x == 0 || y == 0
                  then fromIntegral n
                  else upper - lower
                where
                  lower = max (ceiling (fromIntegral (- x * d) / fromIntegral y)) (ceiling (fromIntegral ((y - n) * d) / fromIntegral x))
                  upper = min (floor (fromIntegral ((n - x) * d) / fromIntegral y)) (floor (fromIntegral (y * d) / fromIntegral x))
      ]

main :: IO ()
main = print $ numTriangles 50