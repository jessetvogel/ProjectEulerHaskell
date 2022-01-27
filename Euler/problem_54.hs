{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad
import Data.Char
import Data.List (nub, sort)
import Data.Maybe
import System.IO

-- Custom types
data Suit = Club | Diamond | Heart | Spade deriving (Read, Show, Enum, Eq, Ord)

type CardValue = Integer

data Card = Card {value :: CardValue, suit :: Suit} deriving (Read, Show, Eq, Ord)

data Combination = HighCard | OnePair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush deriving (Read, Show, Enum, Eq, Ord)

type Hand = [Card]

type Evaluation = (Combination, [CardValue])

-- Parsing
toSuit :: Char -> Suit
toSuit 'H' = Heart
toSuit 'C' = Club
toSuit 'S' = Spade
toSuit 'D' = Diamond

toCardValue :: Char -> CardValue
toCardValue '2' = 2
toCardValue '3' = 3
toCardValue '4' = 4
toCardValue '5' = 5
toCardValue '6' = 6
toCardValue '7' = 7
toCardValue '8' = 8
toCardValue '9' = 9
toCardValue 'T' = 10
toCardValue 'J' = 11
toCardValue 'Q' = 12
toCardValue 'K' = 13
toCardValue 'A' = 14

parseHands :: String -> (Hand, Hand)
parseHands s = splitAt 5 cards where cards = map (\[val, suit] -> Card (toCardValue val) (toSuit suit)) (words s)

-- Util
firstJust :: [Maybe a] -> Maybe a
firstJust m =
  if isJust h
    then h
    else firstJust (tail m)
  where
    h = head m

maybeTake :: Int -> [a] -> Maybe [a]
maybeTake n l = if length l >= n then Just (take n l) else Nothing

maybeConcat :: Maybe [[a]] -> Maybe [a]
-- maybeConcat Nothing = Nothing
-- maybeConcat (Just l) = Just (concat l)
maybeConcat = fmap concat

findTuples :: Int -> Int -> Hand -> Maybe [Card]
findTuples n k h =
  maybeConcat $
    maybeTake k $
      nub $
        filter (\l -> length l == n) $
          map (\c -> filter (\c' -> value c' == value c) h) h

-- Hand evaluation functions
evaluateHand :: Hand -> Evaluation
evaluateHand h =
  fromJust $
    firstJust
      [ findRoyalFlush h,
        findStraightFlush h,
        findFourOfAKind h,
        findFullHouse h,
        findFlush h,
        findStraight h,
        findThreeOfAKind h,
        findTwoPairs h,
        findPair h,
        findHighCard h
      ]

findPair :: Hand -> Maybe Evaluation
findPair h =
  if isJust p
    then Just (OnePair, v : (reverse $ sort $ filter (/= v) $ map value h))
    else Nothing
  where
    p = findTuples 2 1 h
    v = value $ head $ fromJust p

findThreeOfAKind :: Hand -> Maybe Evaluation
findThreeOfAKind h =
  if isJust t
    then Just (ThreeOfAKind, v : (reverse $ sort $ filter (/= v) $ map value h))
    else Nothing
  where
    t = findTuples 3 1 h
    v = value $ head $ fromJust t

findTwoPairs :: Hand -> Maybe Evaluation
findTwoPairs h =
  if isJust pp
    then Just (TwoPairs, [v1, v2] ++ [head $ filter (\v -> v /= v1 && v /= v2) $ map value h])
    else Nothing
  where
    pp = findTuples 2 2 h
    [v1, v2] = reverse $ sort $ take 2 $ nub $ map value $ fromJust pp

findStraight :: Hand -> Maybe Evaluation
findStraight h =
  -- Look for five different values, which are 4 apart
  if max - min == 4 && length (nub values) == 5
    then Just (Straight, [max])
    else Nothing
  where
    values = map value h
    max = maximum values
    min = minimum values

findFlush :: Hand -> Maybe Evaluation
findFlush h =
  -- Look for a single suit
  if length suits == 1
    then Just (Flush, reverse (sort (map value h)))
    else Nothing
  where
    suits = nub (map suit h)

findFullHouse :: Hand -> Maybe Evaluation
findFullHouse h =
  -- A full house consists of a pair + three of a kind
  if isJust t && isJust p
    then Just (FullHouse, [vt, vp])
    else Nothing
  where
    t = findThreeOfAKind h
    p = findPair h
    vt = head $ snd (fromJust t)
    vp = head $ snd (fromJust p)

findFourOfAKind :: Hand -> Maybe Evaluation
findFourOfAKind h =
  if isJust f
    then Just (FourOfAKind, [v, head (filter (/= v) (map value h))])
    else Nothing
  where
    f = findTuples 4 1 h
    v = value $ head $ fromJust f

findStraightFlush :: Hand -> Maybe Evaluation
-- A straight flush is just a straight + flush
findStraightFlush h =
  if isJust s && isJust f
    then Just (StraightFlush, [maximum (map value h)])
    else Nothing
  where
    s = findStraight h
    f = findFlush h

findRoyalFlush :: Hand -> Maybe Evaluation
-- A royal flush is a straight flush where the highest card is an ace
findRoyalFlush h =
  if isJust sf && maximum (map value h) == toCardValue 'A'
    then Just (RoyalFlush, [])
    else Nothing
  where
    sf = findStraightFlush h

findHighCard :: Hand -> Maybe Evaluation
findHighCard h = Just (HighCard, reverse (sort (map value h)))

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2 = compare (evaluateHand h1) (evaluateHand h2)

-- Read file contents
fileGetContents :: String -> IO String
fileGetContents path = do
  handle <- openFile path ReadMode
  hGetContents handle

main :: IO ()
main = do
  -- Read data file
  contents <- fileGetContents "data/data_54.txt"
  -- Parse each line to hands
  let hands = map parseHands (lines contents)
  -- Determine winning hands
  let winningHandsForPlayer1 = filter (\(h1, h2) -> compareHands h1 h2 == GT) hands
  -- Print stuff
  print $ length winningHandsForPlayer1
