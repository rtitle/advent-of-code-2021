import Data.List.Split

move :: String -> (Int, Int)
move s = let 
    [dir, n] = splitOn " " s 
    n' = read n in
        case dir of
            "up" -> (0, -n')
            "down" -> (0, n')
            "forward" -> (n', 0)
            otherwise -> (0, 0)

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 x y = (fst x + fst y, snd x + snd y)

aim :: (Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
aim (x1, x2) (y1, y2, y3) = (y1 + x1, y2 + x2, y3 + x1 * y2)

main :: IO ()
main = do
    input <- readFile "day2-input.txt"
    let moves = fmap move (lines input)
    let (h, d) = foldr add2 (0,0) moves
    let part1 = h * d
    let (h2, _, d2) = foldr aim (0,0,0) (reverse moves)
    let part2 = h2 * d2
    print $ part1
    print $ part2
