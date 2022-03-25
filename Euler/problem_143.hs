import Euler.Util (isSquare)

-- Must have:
-- (1) a^2 = q^2 + r^2 + qr
-- (2) b^2 = p^2 + q^2 + pq
-- (3) c^2 = p^2 + r^2 + pr

m :: Integer
m = 120000

-- Find all pairs (u, v) such that u^2 + v^2 + uv is a square
uvs :: [(Integer, Integer)]
uvs = [(u, v) | u <- [1 .. m], v <- [u + 1 .. m - u], isSquare (u ^ 2 + v ^ 2 + u * v)]

main :: IO ()
main = print $ length uvs
