import Data.ByteString (elemIndex)
fibonacci :: [Integer]
fibonacci = [1, 1] ++ [a + b | (a, b) <- zip fibonacci (tail fibonacci)]

indexSatisfying :: (a -> Bool) -> [a] -> Integer
indexSatisfying cond list = if cond (head list) then 0 else 1 + indexSatisfying cond (tail list)

main :: IO ()
main = print $ 1 + indexSatisfying (\i -> length (show i) >= 1000) fibonacci
