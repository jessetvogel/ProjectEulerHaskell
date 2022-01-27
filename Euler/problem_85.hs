-- Take a rectangle of size W x H
-- How many subrectangles of size w x h ?
-- Well, horizontally, W - w + 1 possibilities
-- and vertically H - h + 1 possibilities
-- so in total (W - w + 1) (H - h + 1) possibilities
countRectangles :: Int -> Int -> Integer
countRectangles w h = sum [fromIntegral $ (w - x + 1) * (h - y + 1) | x <- [1 .. w], y <- [1 .. h]]

main :: IO ()
main = print $ head [ (w, h, c, w * h) | w <- [1 ..], h <- [1 .. w], let c = countRectangles w h, abs (c - 2000000) < 100]
