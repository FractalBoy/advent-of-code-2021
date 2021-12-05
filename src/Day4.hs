module Day4 (part1, part2) where

import Data.List

data BingoCell = BingoCell {number :: Int, marked :: Bool}

type BingoRow = [BingoCell]

type BingoCard = [BingoRow]

type BingoFunction = [Int] -> [BingoCard] -> (Int, Maybe BingoCard)

part1 :: [String] -> Maybe String
part1 = solve playBingoUntilSomeoneWins

part2 :: [String] -> Maybe String
part2 = solve playBingoUntilEveryoneWins

solve :: BingoFunction -> [String] -> Maybe String
solve bingoFunction input = do
  let (drawings, cards) = readInput input
  let (drawing, winner) = bingoFunction drawings cards
  show . scoreWinner drawing <$> winner

scoreWinner :: Int -> BingoCard -> Int
scoreWinner drawing rows = drawing * sum (map scoreRow rows)

scoreRow :: BingoRow -> Int
scoreRow cells = sum $ map scoreCell cells

scoreCell :: BingoCell -> Int
scoreCell cell = if not $ marked cell then number cell else 0

playBingoUntilSomeoneWins :: [Int] -> [BingoCard] -> (Int, Maybe BingoCard)
playBingoUntilSomeoneWins (drawing : drawings) cards =
  let cards' = performBingoRound drawing cards
   in if foundWinner cards'
        then (drawing, find isWinner cards')
        else playBingoUntilSomeoneWins drawings cards'
playBingoUntilSomeoneWins _ _ = undefined

playBingoUntilEveryoneWins :: [Int] -> [BingoCard] -> (Int, Maybe BingoCard)
playBingoUntilEveryoneWins (drawing : drawings) cards =
  let cards' = performBingoRound drawing cards
   in if allWinners cards'
        then (drawing, flipCard drawing <$> find (not . isWinner) cards)
        else playBingoUntilEveryoneWins drawings cards'
playBingoUntilEveryoneWins _ _ = undefined

foundWinner :: [BingoCard] -> Bool
foundWinner = any isWinner

allWinners :: [BingoCard] -> Bool
allWinners = all isWinner

isWinner :: BingoCard -> Bool
isWinner card = hasWinningRow card || hasWinningColumn card

hasWinningRow :: BingoCard -> Bool
hasWinningRow = any isWinningRow

isWinningRow :: BingoRow -> Bool
isWinningRow = all marked

hasWinningColumn :: BingoCard -> Bool
hasWinningColumn = hasWinningRow . transpose

performBingoRound :: Int -> [BingoCard] -> [BingoCard]
performBingoRound n = map (flipCard n)

flipCard :: Int -> BingoCard -> BingoCard
flipCard n = map (flipRow n)

flipRow :: Int -> BingoRow -> BingoRow
flipRow n = map (flipCell n)

flipCell :: Int -> BingoCell -> BingoCell
flipCell n cell = if number cell == n then cell {marked = True} else cell

readInput :: [String] -> ([Int], [BingoCard])
readInput [] = ([], [])
readInput (drawingsText : cardsText) = (drawings, cards)
  where
    drawings = parseDrawings drawingsText
    cards = map parseCard $ readCards (tail cardsText)

readCards :: [String] -> [[String]]
readCards = split ""

parseCard :: [String] -> BingoCard
parseCard = reverse . foldl (\acc x -> createRow (words x) : acc) []
  where
    createCell cell = BingoCell {number = read cell, marked = False}
    createRow = map createCell

parseDrawings :: String -> [Int]
parseDrawings = map read . split ','

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delim string =
  let (before, after) = span (/= delim) string
   in before : split delim (drop 1 after)
