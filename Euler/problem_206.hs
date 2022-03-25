import Control.Monad (when)
import Data.Foldable (for_)

-- Lower bound is 1.000.000.000
-- Upper bound is 1.389.353.824

-- n^2 ends on 0, so n ends on 0
-- 1_2_3_4_5_6_7_8_900
-- (n / 10)^2 ends on 9, so (n / 10) ends on 3 or 7
--

isSolution :: Integer -> Bool
isSolution n =
  let m = n ^ 2
   in and
        [ m `mod` 10 == 0,
          (m `div` 10 ^ 2) `mod` 10 == 9,
          (m `div` 10 ^ 4) `mod` 10 == 8,
          (m `div` 10 ^ 6) `mod` 10 == 7,
          (m `div` 10 ^ 8) `mod` 10 == 6,
          (m `div` 10 ^ 10) `mod` 10 == 5,
          (m `div` 10 ^ 12) `mod` 10 == 4,
          (m `div` 10 ^ 14) `mod` 10 == 3,
          (m `div` 10 ^ 16) `mod` 10 == 2,
          (m `div` 10 ^ 18) `mod` 10 == 1
        ]

main :: IO ()
main = do
  for_
    [1000000000, 1000000010 .. 1389353824]
    ( \n -> do
        when (isSolution n) $ print n
    )
