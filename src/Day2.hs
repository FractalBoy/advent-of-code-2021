module Day2 (part1, part2) where

data Command = Forward Int | Down Int | Up Int

data Location = Location {depth :: Int, horizontal :: Int, aim :: Int}

part1 :: [String] -> String
part1 = show . multiplyLocation . foldl doCommandNoAim Location {depth = 0, horizontal = 0, aim = 0} . parseCommands

part2 :: [String] -> String
part2 = show . multiplyLocation . foldl doCommandWithAim Location {depth = 0, horizontal = 0, aim = 0} . parseCommands

parseCommands :: [String] -> [Command]
parseCommands = reverse . foldl buildCommand [] . map words
  where
    buildCommand acc ("forward" : n : _) = Forward (read n) : acc
    buildCommand acc ("down" : n : _) = Down (read n) : acc
    buildCommand acc ("up" : n : _) = Up (read n) : acc
    buildCommand _ _ = undefined

doCommandNoAim :: Location -> Command -> Location
doCommandNoAim loc (Forward x) = loc {horizontal = horizontal loc + x}
doCommandNoAim loc (Down x) = loc {depth = depth loc + x}
doCommandNoAim loc (Up x) = loc {depth = depth loc - x}

doCommandWithAim :: Location -> Command -> Location
doCommandWithAim loc (Forward x) = loc {horizontal = horizontal loc + x, depth = depth loc + aim loc * x}
doCommandWithAim loc (Down x) = loc {aim = aim loc + x}
doCommandWithAim loc (Up x) = loc {aim = aim loc - x}

multiplyLocation :: Location -> Int
multiplyLocation (Location y x _) = y * x
