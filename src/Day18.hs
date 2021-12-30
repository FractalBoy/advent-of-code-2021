module Day18 (part1) where

import Control.Applicative

example =
  [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
    "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
    "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
    "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
    "[7,[5,[[3,8],[1,4]]]]",
    "[[2,[2,2]],[8,[8,1]]]",
    "[2,9]",
    "[1,[[[9,3],9],[[9,0],[0,7]]]]",
    "[[[5,[7,4]],7],1]",
    "[[[[4,2],2],6],[8,7]]"
  ]

example2 =
  [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
    "[[[5,[2,8]],4],[5,[[9,9],0]]]",
    "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
    "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
    "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
    "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
    "[[[[5,4],[7,7]],8],[[8,3],8]]",
    "[[9,3],[[9,9],[6,[4,9]]]]",
    "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
    "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
  ]

data SnailfishNumber
  = Pair SnailfishNumber SnailfishNumber
  | Number Int

instance Show SnailfishNumber where
  show (Number num) = show num
  show (Pair left right) = "[" ++ show left ++ "," ++ show right ++ "]"

data Breadcrumb = L SnailfishNumber | R SnailfishNumber deriving (Show)

type Breadcrumbs = [Breadcrumb]

type Zipper = (SnailfishNumber, Breadcrumbs)

part1 :: [String] -> String
part1 (line : lines) = show $ part1' (parseSnailfishNumber line) lines
  where
    part1' pair (line : lines) = part1' (reduce (Pair pair (parseSnailfishNumber line))) lines
    part1' pair [] = magnitude pair
part1 _ = ""

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
  (num, breadcrumbs) <- findSplitFocus (n, [])

  splitted <- splitNumber num
  return $ fst $ goToTop (splitted, breadcrumbs)

splitNumber :: SnailfishNumber -> Maybe SnailfishNumber
splitNumber num@(Number n)
  | n >= 10 = Just $ Pair (Number (floor (fromIntegral n / 2))) (Number (ceiling (fromIntegral n / 2)))
  | otherwise = Just num
splitNumber _ = Nothing

explode :: SnailfishNumber -> Maybe SnailfishNumber
explode num = do
  zipper@(Pair (Number left) (Number _), _) <- findExplodeFocus (num, [])

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
