module Day18 (part1, part2) where

import Control.Applicative

data SnailfishNumber
  = Pair SnailfishNumber SnailfishNumber
  | Number Int

instance Show SnailfishNumber where
  show (Number num) = show num
  show (Pair left right) = "[" ++ show left ++ "," ++ show right ++ "]"

instance Eq SnailfishNumber where
  (Number num1) == (Number num2) = num1 == num2
  (Pair _ _) == (Number _) = False
  (Number _) == (Pair _ _) = False
  (Pair left1 right1) == (Pair left2 right2) = left1 == left2 && right1 == right2
  num1 /= num2 = not (num1 == num2)

data Breadcrumb = L SnailfishNumber | R SnailfishNumber deriving (Show)

type Breadcrumbs = [Breadcrumb]

type Zipper = (SnailfishNumber, Breadcrumbs)

part1 :: [String] -> String
part1 (line : lines) = show $ part1' (parseSnailfishNumber line) lines
  where
    part1' pair (line : lines) = part1' (reduce (Pair pair (parseSnailfishNumber line))) lines
    part1' pair [] = magnitude pair
part1 _ = ""

part2 :: [String] -> String
part2 lines =
  let numbers = map parseSnailfishNumber lines
   in show $
        maximum $
          map (\(left, right) -> magnitude $ reduce (Pair left right)) $
            filter (uncurry (/=)) $ (,) <$> numbers <*> numbers

magnitude :: SnailfishNumber -> Int
magnitude (Pair left right) = 3 * magnitude left + 2 * magnitude right
magnitude (Number num) = num

reduce :: SnailfishNumber -> SnailfishNumber
reduce pair = maybe pair reduce (explode pair <|> split pair)

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

findExplodeFocus :: Zipper -> Maybe Zipper
findExplodeFocus zipper@(Pair left right, crumbs) =
  if length crumbs == 4
    then Just zipper
    else (goLeft zipper >>= findExplodeFocus) <|> goRight zipper >>= findExplodeFocus
findExplodeFocus (_, []) = Nothing
findExplodeFocus (Number _, _) = Nothing

findSplitFocus :: Zipper -> Maybe Zipper
findSplitFocus zipper@(Number n, _)
  | n >= 10 = Just zipper
  | otherwise = Nothing
findSplitFocus zipper@(Pair left right, _) =
  (goLeft zipper >>= findSplitFocus)
    <|> goRight zipper >>= findSplitFocus

split :: SnailfishNumber -> Maybe SnailfishNumber
split n = do
  (Number num, breadcrumbs) <- findSplitFocus (n, [])

  let half = fromIntegral num / 2

  return $ fst $ goToTop (Pair (Number (floor half)) (Number (ceiling half)), breadcrumbs)

explode :: SnailfishNumber -> Maybe SnailfishNumber
explode num = do
  zipper@(Pair (Number left) (Number _), _) <- findExplodeFocus (num, [])

  -- In the next two steps, we find the rightmost and leftmost numbers and add the appropriate
  -- value to each, then go back to the top of the tree and find the exploded pair again.
  zipper'@(Pair (Number _) (Number right), _) <-
    ( (findRightmostOnLeft zipper >>= addToZipper left)
        >>= findExplodeFocus . goToTop
      )
      <|> Just zipper

  (_, breadcrumbs) <-
    ( (findLeftmostOnRight zipper' >>= addToZipper right)
        >>= findExplodeFocus . goToTop
      )
      <|> Just zipper'

  return $ fst $ goToTop (Number 0, breadcrumbs)

addToZipper :: Int -> Zipper -> Maybe Zipper
addToZipper _ (Pair _ _, _) = Nothing
addToZipper toAdd (Number num, breadcrumbs) = Just (Number (num + toAdd), breadcrumbs)

findRightmostOnLeft :: Zipper -> Maybe Zipper
findRightmostOnLeft zipper@(_, (L _) : crumbs) = goUp zipper >>= findRightmostOnLeft
findRightmostOnLeft zipper@(_, (R _) : crumbs) = goUp zipper >>= goLeft >>= findRightmostNumber
findRightmostOnLeft zipper@(_, []) = Nothing

findRightmostNumber :: Zipper -> Maybe Zipper
findRightmostNumber zipper@(Pair _ _, _) = goRight zipper >>= findRightmostNumber
findRightmostNumber zipper@(Number _, _) = Just zipper

findLeftmostOnRight :: Zipper -> Maybe Zipper
findLeftmostOnRight zipper@(_, (R _) : crumbs) = goUp zipper >>= findLeftmostOnRight
findLeftmostOnRight zipper@(_, (L _) : crumbs) = goUp zipper >>= goRight >>= findLeftmostNumber
findLeftmostOnRight zipper@(_, []) = Nothing

findLeftmostNumber :: Zipper -> Maybe Zipper
findLeftmostNumber zipper@(Pair _ _, _) = goLeft zipper >>= findLeftmostNumber
findLeftmostNumber zipper@(Number _, _) = Just zipper

goLeft :: Zipper -> Maybe Zipper
goLeft (Pair left right, bs) = Just (left, L right : bs)
goLeft _ = Nothing

goRight :: Zipper -> Maybe Zipper
goRight (Pair left right, bs) = Just (right, R left : bs)
goRight _ = Nothing

goUp :: Zipper -> Maybe Zipper
goUp (left, L right : bs) = Just (Pair left right, bs)
goUp (right, R left : bs) = Just (Pair left right, bs)
goUp (_, []) = Nothing

goToTop :: Zipper -> Zipper
goToTop zipper@(_, []) = zipper
goToTop zipper = maybe zipper goToTop (goUp zipper)
