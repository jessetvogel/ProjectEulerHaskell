import Euler.Util (fileGetContents)

codes :: IO [String]
codes = lines <$> fileGetContents "data/data_79.txt"

containsInOrder :: Eq a => [a] -> [a] -> Bool
containsInOrder l [] = True
containsInOrder [] m = False
containsInOrder l m =
  if head m == head l
    then containsInOrder (tail l) (tail m)
    else containsInOrder (tail l) m

main :: IO ()
main = do
  c <- codes
  let passcode = head [n | n <- [1 ..], all (containsInOrder (show n)) c]
  print passcode
