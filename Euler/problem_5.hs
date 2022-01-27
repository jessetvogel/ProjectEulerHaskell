-- Print the lcm of 1 .. 20
main :: IO ()
main = print (foldl lcm 1 [1..20])
