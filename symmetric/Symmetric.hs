module Main (main) where

import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= processArgs

usageMessage = "usage: pebsym <depth> <amount>"

processArgs :: [String] -> IO ()
processArgs []       = putStrLn usageMessage
processArgs (d:a:[]) = putStrLn $ listAll d a
processArgs _        = putStrLn usageMessage

listAll :: String -> String -> String
listAll d a = maybe usageMessage id result
  where
    result = readMaybe d >>= \d ->
             readMaybe a >>= \a ->
             return $ showAll $ allValid d a

showAll :: [Tree] -> String
showAll = legend . intercalate "\n" . map show
  where legend = ("Results (root marked by \"r\"):\n r\n" ++)

-- `allValid depth amt` generates all pebbleable ("valid"; see below) Trees
-- having a given depth and using no more than the given amount of pebbles.
allValid :: Int -> Int -> [Tree]
allValid d amt = filter valid $ candidates d amt

-- Since these binary trees are "symmetrical" (i.e. each node at a given depth
-- has the same amount of material), they can be represented as simple lists.
type Tree = [Int]

-- Coincidentally, we also represent paths as lists of Ints.
type Path = [Int]

-- `candidates depth amt` generates all Trees having the given depth and using
-- no more than the given amount of pebbles.
candidates :: Int -> Int -> [Tree]
candidates 0 amt = [[amt]]
candidates d amt = [ (n:ns) | n <- [0..amt],
                              ns <- candidates (d-1) ((amt-n) `div` 2) ]

-- `valid tree` is True if the given Tree is pebbleable.
valid :: Tree -> Bool
valid = pebbles . collapse

-- `pebbles path` is True if the Path is pebbleable (in our usual domination
-- sense).
pebbles :: Path -> Bool
pebbles = help 0
  where
    help _        []     = True
    help fromLeft (n:ns) = let toN = fromLeft + n
                           in toN + (muster ns `div` 2) > 0
                           && help (toN `div` 2) ns

-- `collapse tree` transforms a Tree into an equivalent path; that is, if the
-- path is pebbleable, then so is the original Tree.
collapse :: Tree -> Path
collapse []          = []
collapse (top:below) = top' : collapse below
  where top' = top + (muster below `div` 2)

-- `muster t/p` is the amount of material that can be transported to the root
-- of a Tree or Path. We make use of this somewhat undesirable pun above in a
-- particularly despicable manner.
muster :: [Int] -> Int
muster = foldr halve 0
  where halve node below = node
                         + 2 * (below `div` 2)
