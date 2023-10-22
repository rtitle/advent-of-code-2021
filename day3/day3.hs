import Data.Char
import Data.List

mostCommon :: [String] -> String
mostCommon s = fmap mc (transpose s) where
    mc s = if zeros s > ones s then '0' else '1'
    ones = length . filter (=='1')
    zeros = length . filter (=='0')

invert :: String -> String
invert = fmap inv where
    inv '1' = '0'
    inv '0' = '1'

keep :: [String] -> Bool -> Int -> String
keep [s] _ _ = s
keep s inv n = keep s' inv (n+1) where
    s' = filter (\x -> x !! n == criteria !! n) s
    mc = mostCommon s
    criteria = if inv then invert mc else mc

toDecimal :: String -> Int
toDecimal s = fst $ foldr fn (0, 1) s where
    fn c (res, pow) = (res + pow * c', pow * 2) where
        c' = digitToInt c

main :: IO ()
main = do
    input <- readFile "day3-input.txt"
    let ls = lines input
    let gamma = mostCommon ls
    let epsilon = invert gamma
    let part1 = (toDecimal gamma) * (toDecimal epsilon)
    print $ part1
    let oxygen = keep ls False 0
    let co2 = keep ls True 0
    let part2 = (toDecimal oxygen) * (toDecimal co2)
    print $ part2