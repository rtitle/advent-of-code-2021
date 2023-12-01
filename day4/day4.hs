import Data.List
import Data.List.Split
import Data.Ord

type Board = ([(Int, Bool)], Int)
type Game = [Int]

isWinner :: Board -> Bool
isWinner b = any (all (\x -> (snd ((fst b) !! x)) == True)) indexes where
    indexes = [
        [0..4], 
        [5..9], 
        [10..14], 
        [15..19], 
        [20..24], 
        fmap (*5) [0..4], 
        fmap (\i -> i * 5 + 1) [0..4], 
        fmap (\i -> i * 5 + 2) [0..4], 
        fmap (\i -> i * 5 + 3) [0..4], 
        fmap (\i -> i * 5 + 4) [0..4]]

playUntilWinner :: [Board] -> Game -> Board
playUntilWinner bs g = head $ foldl update bs (zip [0..] g) where
    update r (i,c) = case filter isWinner r of 
        (h:t) -> [h]
        [] -> fmap (updateBoard c i) r
    updateBoard n i (b,_) = ((fmap (\x -> if (fst x) == n then (n, True) else x) b), i)

playAll :: [Board] -> Game -> Board
playAll bs g = maximumBy (comparing snd) $ foldl update bs (zip [0..] g) where
    update r (i,c) = markWinner i (fmap (updateBoard c i) r)
    updateBoard n i b = if snd b == 0 then ((fmap (\x -> if (fst x) == n then (n, True) else x) (fst b)), 0) else b
    markWinner i = fmap (\u -> if (isWinner u) && (snd u) == 0 then (fst u, i) else u)

parseBoard :: [String] -> Board
parseBoard s = let splitted = fmap (splitOn " ") s in
    (((fmap (,False)) . (fmap parseInt) . filter (not . null) . concat $ splitted), 0)

parseGame :: String -> Game
parseGame s = fmap parseInt (splitOn "," s)

parseInt :: String -> Int
parseInt s = read s :: Int

result :: Game -> Board -> Int
result g (b, i) = (g !! i) * (sum (fmap (\x -> if (snd x) == False then fst x else 0) b))

main :: IO ()
main = do
    input <- readFile "day4-input.txt"
    let ls = splitWhen null $ lines input
    let game = parseGame (head . head $ ls)
    let boards = fmap parseBoard (drop 1 ls)
    let p1 = result game (playUntilWinner boards game)
    let p2 = result game (playAll boards game)
    print $ p1
    print $ p2