module Day10 (part1, part2) where

import Data.List

part1 :: [String] -> String
part1 input = show $ sum $ fmap (calculateScore . fst . findUnmatchingBrace) input

part2 :: [String] -> String
part2 =
  show
    . getMiddleValue
    . sort
    . map (calculateCompletionScore . findCompletionBraces)
    . filter (not . null)
    . map
      ( \str -> case findUnmatchingBrace str of
          (Nothing, stack) -> stack
          (Just _, _) -> ""
      )

getMiddleValue :: [a] -> a
getMiddleValue xs = xs !! (length xs `div` 2)

calculateCompletionScore :: String -> Int
calculateCompletionScore =
  foldl
    ( \score c ->
        score * 5
          + ( case c of
                ')' -> 1
                ']' -> 2
                '}' -> 3
                '>' -> 4
                _ -> 0
            )
    )
    0

findCompletionBraces :: String -> String
findCompletionBraces =
  reverse
    . foldl
      ( \acc b -> case b of
          '(' -> ')' : acc
          '[' -> ']' : acc
          '{' -> '}' : acc
          '<' -> '>' : acc
          _ -> acc
      )
      ""

calculateScore :: Maybe Char -> Int
calculateScore (Just c) = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> 0
calculateScore Nothing = 0

findUnmatchingBrace :: String -> (Maybe Char, String)
findUnmatchingBrace = findUnmatchingBrace' []
  where
    findUnmatchingBrace' stack (c : str) = case c of
      '(' -> pushStack
      ')' -> isValidClose '(' ')'
      '[' -> pushStack
      ']' -> isValidClose '[' ']'
      '{' -> pushStack
      '}' -> isValidClose '{' '}'
      '<' -> pushStack
      '>' -> isValidClose '<' '>'
      _ -> undefined
      where
        isValidClose open close = if not (null stack) && (head stack /= open) then (Just close, stack) else findUnmatchingBrace' (tail stack) str
        pushStack = findUnmatchingBrace' (c : stack) str
    findUnmatchingBrace' stack [] = if null stack then (Nothing, "") else (Nothing, stack)
