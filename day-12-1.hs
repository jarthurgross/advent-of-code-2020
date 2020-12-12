import Data.List
import Data.List.Split
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Math.Sequence.Converge

main = do
  contents <- getContents
  putStrLn $ show $ manhattanDist $ snd $ foldl' moveShip (0, (0, 0)) $ map splitCharInt $ lines contents

moveShip :: (Int, (Int, Int)) -> (Char, Int) -> (Int, (Int, Int))
moveShip (d, (x, y)) (c, a)
  | c == 'N' = (d, (x, y + a))
  | c == 'S' = (d, (x, y - a))
  | c == 'E' = (d, (x + a, y))
  | c == 'W' = (d, (x - a, y))
  | c == 'R' = ((d - a) `mod` 360, (x, y))
  | c == 'L' = ((d + a) `mod` 360, (x, y))
  | c == 'F' = (d, forward d (x, y))
  | otherwise = (d, (x, y))
  where forward d (x, y)
          | d == 0 = (x + a, y)
          | d == 90 = (x, y + a)
          | d == 180 = (x - a, y)
          | d == 270 = (x, y - a)
          | otherwise = (x, y)

splitCharInt :: String -> (Char, Int)
splitCharInt s = (head s, readInt $ tail s)

manhattanDist :: (Int, Int) -> Int
manhattanDist (x, y) = (abs x) + (abs y)

readInt :: String -> Int
readInt = read

takeDigits :: String -> (String, String)
takeDigits "" = ("", "")
takeDigits (c:cs) = if c `elem` ['0' .. '9'] then (c:ds, ls) else ("", c:cs)
  where (ds, ls) = takeDigits cs

count :: Eq a => a -> [a] -> Int
count c = foldr (\c' -> \n -> if c == c' then n + 1 else n) 0

stride :: Int -> [a] -> [a]
stride n = (map head) . (chunksOf n)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
