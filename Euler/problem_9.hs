-- Construct the list of relevant triplets with a < b (should be a single one)
triplets = [[a, b, c] |
   b <- [1 .. 1000],
   a <- [1 .. b],
   let c = 1000 - a - b,
   a ^ 2 + b ^ 2 == c^2 ]

-- Print the product abc
main :: IO ()
main = print (product (head triplets))
