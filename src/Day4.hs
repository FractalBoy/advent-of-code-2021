module Day4 where

import Data.List
import Data.String
import GHC.IO.Encoding.Failure (codingFailureModeSuffix)

data BingoCell = BingoCell {number :: Int, marked :: Bool}

instance Show BingoCell where
  show (BingoCell n True) = show n ++ "-x"
  show (BingoCell n False) = show n ++ "-o"

newtype BingoRow = BingoRow [BingoCell]

instance Show BingoRow where
  show (BingoRow xs) = unwords $ map show xs

newtype BingoCard = BingoCard [BingoRow]

instance Show BingoCard where
  show (BingoCard xs) = unlines $ map show xs

part1 :: [String] -> Maybe String
part1 input = do
  let (drawings, cards) = readInput input
  winner <- getWinner drawings cards
  return $ show $ scoreWinner winner

getWinner :: [Int] -> [BingoCard] -> Maybe (Int, BingoCard)
getWinner drawings cards = do
  let (drawing, cards') = playBingo drawings cards
  winner <- find isWinner cards'
  return (drawing, winner)

scoreWinner :: (Int, BingoCard) -> Int
scoreWinner (drawing, BingoCard rows) = drawing * sum (map scoreRow rows)

scoreRow :: BingoRow -> Int
scoreRow (BingoRow cells) = sum $ map scoreCell cells

scoreCell :: BingoCell -> Int
scoreCell cell = if not $ marked cell then number cell else 0

playBingo :: [Int] -> [BingoCard] -> (Int, [BingoCard])
playBingo = playBingo' 0
  where
    playBingo' n [] cards = (n, cards)
    playBingo' n (drawing : drawings) cards = let cards' = performBingoRound drawing cards in if foundWinner cards then (n, cards) else playBingo' drawing drawings cards'

foundWinner :: [BingoCard] -> Bool
foundWinner = any isWinner

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
flipCell n cell = let (BingoCell number _) = cell in if n == number then BingoCell number True else cell

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
