module Day2 (part1, part2) where

data Command = Forward Int | Down Int | Up Int

data Location = Location {depth :: Int, horizontal :: Int, aim :: Int}

part1 :: [String] -> String
part1 = executeCommands doCommandWithoutAim

part2 :: [String] -> String
part2 = executeCommands doCommandWithAim

executeCommands :: (Location -> Command -> Location) -> [String] -> String
executeCommands doCommand = show . multiplyLocation . foldl doCommand Location {depth = 0, horizontal = 0, aim = 0} . parseCommands

parseCommands :: [String] -> [Command]
parseCommands = reverse . foldl buildCommand [] . map words
  where
    buildCommand acc ("forward" : n : _) = Forward (read n) : acc
    buildCommand acc ("down" : n : _) = Down (read n) : acc
    buildCommand acc ("up" : n : _) = Up (read n) : acc
    buildCommand _ _ = undefined

doCommandWithoutAim :: Location -> Command -> Location
doCommandWithoutAim loc (Forward x) = loc {horizontal = horizontal loc + x}
doCommandWithoutAim loc (Down x) = loc {depth = depth loc + x}
doCommandWithoutAim loc (Up x) = loc {depth = depth loc - x}

doCommandWithAim :: Location -> Command -> Location
doCommandWithAim loc (Forward x) = loc {horizontal = horizontal loc + x, depth = depth loc + aim loc * x}
doCommandWithAim loc (Down x) = loc {aim = aim loc + x}
doCommandWithAim loc (Up x) = loc {aim = aim loc - x}

multiplyLocation :: Location -> Int
multiplyLocation (Location d h _) = d * h
