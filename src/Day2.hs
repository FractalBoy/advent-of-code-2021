module Day2 (part1) where

data Command = Forward Int | Down Int | Up Int

data Location = Location {depth :: Int, horizontal :: Int}

part1 :: [String] -> String
part1 = show . multiplyLocation . foldl doCommand Location {depth = 0, horizontal = 0} . parseCommands

parseCommands :: [String] -> [Command]
parseCommands = reverse . foldl buildCommand [] . map words
  where
    buildCommand acc ("forward" : n : _) = Forward (read n) : acc
    buildCommand acc ("down" : n : _) = Down (read n) : acc
    buildCommand acc ("up" : n : _) = Up (read n) : acc
    buildCommand _ _ = undefined

doCommand :: Location -> Command -> Location
doCommand loc (Forward x) = loc {horizontal = horizontal loc + x}
doCommand loc (Down x) = loc {depth = depth loc + x}
doCommand loc (Up x) = loc {depth = depth loc - x}

multiplyLocation :: Location -> Int
multiplyLocation (Location y x) = y * x
