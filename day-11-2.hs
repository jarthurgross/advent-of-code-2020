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
  putStrLn $ show $ mapCount '#' $ converge $ iterate updateSeats $ loadSeats $ lines contents

loadSeats :: [String] -> Map (Int, Int) Char
loadSeats ls = snd $ foldl' loadRow (0, Map.empty) ls

loadRow :: (Int, Map (Int, Int) Char) -> String -> (Int, Map (Int, Int) Char)
loadRow (r, m) s = (r + 1, snd $ foldl' loadSeat ((r, 0), m) s)

loadSeat :: ((Int, Int), Map (Int, Int) Char) -> Char -> ((Int, Int), Map (Int, Int) Char)
loadSeat ((r, c), m) s = ((r, c + 1), Map.insert (r, c) s m)

showSeats :: Int -> Int -> Map (Int, Int) Char -> String
showSeats rs cs m = unlines $ [showRow cs r m | r <- [0 .. rs - 1]]

showRow :: Int -> Int -> Map (Int, Int) Char -> String
showRow cs r m = [Map.findWithDefault '.' (r, c) m | c <- [0 .. cs - 1]]

updateSeats :: Map (Int, Int) Char -> Map (Int, Int) Char
updateSeats m = Map.mapWithKey (updateSeat m) m

updateSeat :: Map (Int, Int) Char -> (Int, Int) -> Char -> Char
updateSeat m (r, c) s
  | s == 'L' = if occ == 0 then '#' else 'L'
  | s == '#' = if occ >= 5 then 'L' else '#'
  | otherwise = '.'
  where occ = getOccupied m (r, c)

getOccupied :: Map (Int, Int) Char -> (Int, Int) -> Int
getOccupied m (r, c) = count True $ [dirOccupied m (r, c) (dx, dy) | (dx, dy) <- cartProd [-1 .. 1] [-1 .. 1], (dx, dy) /= (0, 0)]

dirOccupied :: Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool
dirOccupied m (x, y) (dx, dy) = (==) '#' $ head $ dropWhile (=='.') [Map.findWithDefault '|' (x + n*dx, y + n*dy) m | n <- [1 .. ]]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(\a b -> (a, b))] <*> xs <*> ys

readInt :: String -> Int
readInt = read

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) (init xs)

takeDigits :: String -> (String, String)
takeDigits "" = ("", "")
takeDigits (c:cs) = if c `elem` ['0' .. '9'] then (c:ds, ls) else ("", c:cs)
  where (ds, ls) = takeDigits cs

count :: Eq a => a -> [a] -> Int
count c = foldr (\c' -> \n -> if c == c' then n + 1 else n) 0

mapCount :: Eq a => a -> Map k a -> Int
mapCount c = Map.foldr (\c' n -> if c == c' then n + 1 else n) 0

stride :: Int -> [a] -> [a]
stride n = (map head) . (chunksOf n)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
