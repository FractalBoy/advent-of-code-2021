module Day18 where

import Data.List
import Data.Maybe

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
    part1' pair [] = pair
part1 _ = ""

reduce :: SnailfishNumber -> SnailfishNumber
reduce pair
  | canExplode pair = reduce (explode pair)
  | canSplit pair = reduce (split pair)
  | otherwise = pair

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
    else
      let zipperLeft = goLeft zipper >>= findExplodeFocus
          zipperRight = goRight zipper >>= findExplodeFocus
       in if isJust zipperLeft then zipperLeft else zipperRight
findExplodeFocus (_, []) = Nothing
findExplodeFocus (Number _, _) = Nothing

canExplode :: SnailfishNumber -> Bool
canExplode n = (isJust . findExplodeFocus) (n, [])

canSplit :: SnailfishNumber -> Bool
canSplit (Number n) = n >= 10
canSplit (Pair left right) = canSplit left || canSplit right

split :: SnailfishNumber -> SnailfishNumber
split pair@(Pair left right)
  | canSplit left = Pair (split left) right
  | canSplit right = Pair left (split right)
  | otherwise = pair
split num@(Number _) = splitNumber num

splitNumber :: SnailfishNumber -> SnailfishNumber
splitNumber num@(Number n)
  | n >= 10 = Pair (Number (floor (fromIntegral n / 2))) (Number (ceiling (fromIntegral n / 2)))
  | otherwise = num
splitNumber _ = undefined

explode :: SnailfishNumber -> SnailfishNumber
explode num =
  let zipper = findExplodeFocus (num, [])
   in case zipper of
        Nothing -> num
        Just (Pair left right, breadcrumbs) ->
          let explodedZipper = (Number 0, changeLeft (changeRight breadcrumbs right) left)
           in fst $ goToTop explodedZipper
        _ -> undefined
  where
    changeLeft :: Breadcrumbs -> SnailfishNumber -> Breadcrumbs
    changeLeft breadcrumbs (Number num) = case findIndex isLeftNumber breadcrumbs of
      Nothing -> case findIndex isLeftPair breadcrumbs of
        Just index ->
          let leftPair = getLeftPair (breadcrumbs !! index)
           in take index breadcrumbs
                ++ [R (changeRightmostNumber leftPair num)]
                ++ drop (index + 1) breadcrumbs
        Nothing -> breadcrumbs
      Just index ->
        let leftNumber = getLeftNumber (breadcrumbs !! index)
         in take index breadcrumbs
              ++ [R (Number (leftNumber + num))]
              ++ drop (index + 1) breadcrumbs
    changeLeft _ _ = undefined
    changeRight :: Breadcrumbs -> SnailfishNumber -> Breadcrumbs
    changeRight breadcrumbs (Number num) = case findIndex isRightNumber breadcrumbs of
      Nothing -> case findIndex isRightPair breadcrumbs of
        Just index ->
          let rightPair = getRightPair (breadcrumbs !! index)
           in take index breadcrumbs
                ++ [L (changeLeftmostNumber rightPair num)]
                ++ drop (index + 1) breadcrumbs
        Nothing -> breadcrumbs
      Just index ->
        let rightNumber = getRightNumber (breadcrumbs !! index)
         in take index breadcrumbs
              ++ [L (Number (rightNumber + num))]
              ++ drop (index + 1) breadcrumbs
    changeRight _ _ = undefined

isLeftNumber :: Breadcrumb -> Bool
isLeftNumber (R (Number _)) = True
isLeftNumber _ = False

isLeftPair :: Breadcrumb -> Bool
isLeftPair (R (Pair _ _)) = True
isLeftPair _ = False

isRightNumber :: Breadcrumb -> Bool
isRightNumber (L (Number _)) = True
isRightNumber _ = False

isRightPair :: Breadcrumb -> Bool
isRightPair (L (Pair _ _)) = True
isRightPair _ = False

getLeftNumber :: Breadcrumb -> Int
getLeftNumber (R (Number left)) = left
getLeftNumber _ = undefined

getLeftPair :: Breadcrumb -> SnailfishNumber
getLeftPair (R pair@(Pair _ _)) = pair
getLeftPair _ = undefined

changeRightmostNumber :: SnailfishNumber -> Int -> SnailfishNumber
changeRightmostNumber num value = fromMaybe num (changeRightmostNumber' num value)
  where
    changeRightmostNumber' (Pair left (Number right)) value = Just (Pair left (Number (right + value)))
    changeRightmostNumber' (Pair left@(Number _) right@(Pair _ _)) value =
      let rightBranch = changeRightmostNumber' right value
       in if isJust rightBranch then rightBranch else Just left
    changeRightmostNumber' (Pair left@(Pair _ _) right@(Pair _ _)) value =
      let rightBranch = changeRightmostNumber' right value
          leftBranch = changeRightmostNumber' left value
       in if isJust rightBranch then rightBranch else leftBranch
    changeRightmostNumber' (Number _) value = Nothing

changeLeftmostNumber :: SnailfishNumber -> Int -> SnailfishNumber
changeLeftmostNumber num value = fromMaybe num (changeLeftmostNumber' num value)
  where
    changeLeftmostNumber' (Pair (Number left) right) value = Just (Pair (Number (left + value)) right)
    changeLeftmostNumber' (Pair left@(Pair _ _) right@(Number _)) value =
      let leftBranch = changeLeftmostNumber' left value
       in if isJust leftBranch then leftBranch else Just right
    changeLeftmostNumber' (Pair left@(Pair _ _) right@(Pair _ _)) value =
      let leftBranch = changeLeftmostNumber' left value
          rightBranch = changeLeftmostNumber' right value
       in if isJust leftBranch then leftBranch else rightBranch
    changeLeftmostNumber' (Number _) value = Nothing

getRightNumber :: Breadcrumb -> Int
getRightNumber (L (Number right)) = right
getRightNumber _ = undefined

getRightPair :: Breadcrumb -> SnailfishNumber
getRightPair (L pair@(Pair _ _)) = pair
getRightPair _ = undefined

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
