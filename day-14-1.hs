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
  putStrLn $ show $ sum $ runInstructions $ map parseLine $ lines contents

data Instruction = Mask String | Mem Int Int deriving (Show)

parseLine :: String -> Instruction
parseLine l = if x !! 1 == 'a' then Mask y else Mem (parseMemAddress x) (readInt y)
  where x:y:_ = splitOn " = " l

parseMemAddress :: String -> Int
parseMemAddress m = readInt $ (splitOneOf "[]" m) !! 1

runInstructions :: [Instruction] -> Map Int Int
runInstructions = snd . (foldl' runInstruction (replicate 36 'X', Map.empty))

runInstruction :: (String, Map Int Int) -> Instruction -> (String, Map Int Int)
runInstruction (_, mem) (Mask mask) = (mask, mem)
runInstruction (mask, mem) (Mem addr val) = (mask, Map.insert addr (applyMask mask val) mem)

intToBitstring :: Int -> String
intToBitstring = reverse . (take 36) . (unfoldr (\x -> Just (head $ show $ x `mod` 2, x `quot` 2)))

bitStringToInt :: String -> Int
bitStringToInt = sum . (zipWith (\x y -> 2^x * (readInt $ [y])) [35, 34 .. 0])

applyStringMask :: String -> String -> String
applyStringMask = zipWith (\m b -> if m == 'X' then b else m)

applyMask :: String -> Int -> Int
applyMask mask = bitStringToInt . (applyStringMask mask) . intToBitstring

readInt :: String -> Int
readInt = read
