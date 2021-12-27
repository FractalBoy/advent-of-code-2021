module Day18 where

data SnailfishNumber
  = Pair SnailfishNumber SnailfishNumber
  | Number Int
  deriving (Show)

parseSnailfishNumber :: String -> SnailfishNumber
parseSnailfishNumber str
  | head str == '[' && last str == ']' =
    let inside = (tail . init) str
     in if head inside == '['
          then
            let (left, right) = splitSnailfishNumber inside
             in Pair (parseSnailfishNumber left) (parseSnailfishNumber right)
          else
            let (left, right) = break (== ',') inside
             in Pair (parseSnailfishNumber left) (parseSnailfishNumber (tail right))
  | otherwise = Number (read str)

splitSnailfishNumber :: String -> (String, String)
splitSnailfishNumber str
  | length str == 3 = ([head str], [last str]) -- All numbers are just one digit
  | otherwise = splitSnailfishNumber' str "" ""
  where
    splitSnailfishNumber' (c : str) stack acc =
      case c of
        '[' ->
          splitSnailfishNumber' str (c : stack) (c : acc)
        ']' ->
          if length stack == 1
            then (reverse (c : acc), tail str)
            else splitSnailfishNumber' str (tail stack) (c : acc)
        _ -> splitSnailfishNumber' str stack (c : acc)
    splitSnailfishNumber' _ _ _ = undefined
