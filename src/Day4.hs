module Day4 (part1, part2) where

import Data.List

data BingoCell = BingoCell {number :: Int, marked :: Bool}

newtype BingoRow = BingoRow [BingoCell]

newtype BingoCard = BingoCard [BingoRow]

type BingoFunction = ([Int] -> [BingoCard] -> (Int, Maybe BingoCard))

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
scoreWinner drawing (BingoCard rows) = drawing * sum (map scoreRow rows)

scoreRow :: BingoRow -> Int
scoreRow (BingoRow cells) = sum $ map scoreCell cells

scoreCell :: BingoCell -> Int
scoreCell cell = if not $ marked cell then number cell else 0

playBingoUntilSomeoneWins :: [Int] -> [BingoCard] -> (Int, Maybe BingoCard)
playBingoUntilSomeoneWins (drawing : drawings) cards = let cards' = performBingoRound drawing cards in if foundWinner cards' then (drawing, find isWinner cards') else playBingoUntilSomeoneWins drawings cards'
playBingoUntilSomeoneWins _ _ = undefined

playBingoUntilEveryoneWins :: [Int] -> [BingoCard] -> (Int, Maybe BingoCard)
playBingoUntilEveryoneWins (drawing : drawings) cards = let cards' = performBingoRound drawing cards in if allWinners cards' then (drawing, flipCard drawing <$> find (not . isWinner) cards) else playBingoUntilEveryoneWins drawings cards'
playBingoUntilEveryoneWins _ _ = undefined

foundWinner :: [BingoCard] -> Bool
foundWinner = any isWinner

allWinners :: [BingoCard] -> Bool
allWinners = all isWinner

isWinner :: BingoCard -> Bool
isWinner card = hasWinningRow card || hasWinningColumn card

hasWinningRow :: BingoCard -> Bool
hasWinningRow (BingoCard rows) = any isWinningRow rows

isWinningRow :: BingoRow -> Bool
isWinningRow (BingoRow row) = all marked row

hasWinningColumn :: BingoCard -> Bool
hasWinningColumn (BingoCard rows) = hasWinningRow $ BingoCard $ map BingoRow $ transpose $ map (\(BingoRow row) -> row) rows

performBingoRound :: Int -> [BingoCard] -> [BingoCard]
performBingoRound n = map (flipCard n)

flipCard :: Int -> BingoCard -> BingoCard
flipCard n (BingoCard rows) = BingoCard $ map (flipRow n) rows

flipRow :: Int -> BingoRow -> BingoRow
flipRow n (BingoRow cells) = BingoRow $ map (flipCell n) cells

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
parseCard = BingoCard . reverse . foldl (\acc x -> createRow (words x) : acc) []
  where
    createCell cell = BingoCell {number = read cell, marked = False}
    createRow = BingoRow . map createCell

parseDrawings :: String -> [Int]
parseDrawings = map read . split ','

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delim string =
  let (before, after) = span (/= delim) string
   in before : split delim (drop 1 after)
