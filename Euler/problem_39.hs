numberOfTriangles :: Int -> Int
numberOfTriangles n = length [1 | a <- [1 .. n], b <- [1 .. a], let c = n - a - b, a * a + b * b == c * c]

main :: IO ()
main = print $ snd $ maximum [(numberOfTriangles p, p) | p <- [1 .. 1000]]
