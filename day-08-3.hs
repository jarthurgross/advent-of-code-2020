import Data.List
import Data.List.Split
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Math.Sequence.Converge

data Operation = Acc | Jmp | Nop | Ter deriving (Eq, Show)

main = do
  contents <- getContents
  putStrLn $ show $ filter (\(ln, _, _) -> ln == 0) $ map runProgram $ map (\program -> (Jmp, 0):program) $ switchJmpsNops (map parseLine $ lines contents)

runProgram :: [(Operation, Int)] -> (Int, Int, Set Int)
runProgram program = last $ takeWhile (\(ln, acc, lns) -> not $ ln `elem` lns) $ (flip iterate) (1, 0, Set.empty) $ takeStep $ program

switchJmpsNops :: [(Operation, Int)] -> [[(Operation, Int)]]
switchJmpsNops program = [(take n program) ++ [switchJmpNop $ program !! n] ++ (drop (n + 1) program) | n <- [0 .. (length program) - 1], fst (program !! n) `elem` [Jmp, Nop]]

switchJmpNop :: (Operation, Int) -> (Operation, Int)
switchJmpNop (Jmp, n) = (Nop, n)
switchJmpNop (Nop, n) = (Jmp, n)
switchJmpNop (op, n) = (op, n)

parseLine :: String -> (Operation, Int)
parseLine ('a':'c':'c':' ':n) = (Acc, readInt n)
parseLine ('j':'m':'p':' ':n) = (Jmp, readInt n)
parseLine ('n':'o':'p':' ':n) = (Nop, readInt n)
parseLine _ = (Nop, 0)

readInt :: String -> Int
readInt ('+':n) = read n
readInt n = read n

takeStep :: [(Operation, Int)] -> (Int, Int, Set Int) -> (Int, Int, Set Int)
takeStep prog (ln, acc, lns)
  | op == Acc = (ln + 1, acc + n, Set.insert ln lns)
  | op == Jmp = (ln + n, acc, Set.insert ln lns)
  | op == Nop = (ln + 1, acc, Set.insert ln lns)
  | op == Ter = (0, acc, lns)
  where (op, n) = if ln < length prog then prog !! ln else (Ter, -1)

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
