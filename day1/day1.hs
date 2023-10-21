import Data.List
import Data.List.Split

countInc :: [Int] -> Int -> Int
countInc [a] n = n
countInc (a:b:xs) n = countInc (b:xs) (if b > a then n+1 else n)

windows :: [Int] -> [Int] -> [Int]
windows (a:b:c:xs) res = windows (b:c:xs) ((a+b+c):res)
windows _ res = reverse res

str2int :: String -> Int
str2int n = read n :: Int

main :: IO ()
main = 
  do input <- readFile "day1-input.txt"
     let ns = fmap str2int $ lines input
     let part1 = countInc ns 0
     let ws = windows ns []
     let part2 = countInc ws 0
     print $ part1
     print $ part2