import Euler.Util (fileGetContents, splitBy)

main :: IO ()
main = do
  -- Load file
  pairs <- zip [1 ..] . map (map read . splitBy ',') . lines <$> fileGetContents "data/data_99.txt"
  -- Print number
  print $ snd $ maximum [(log base * exp, n) | (n, [base, exp]) <- pairs]
