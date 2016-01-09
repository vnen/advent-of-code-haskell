import System.IO (readFile)
import Data.List (nub, union)

type Coords = ([(Int, Int)], [(Int, Int)])

main = do
    input <- readFile "input3.txt"
    putStrLn $ show (houses input)

houses :: String -> Int
houses input =
    let (santa, robo) = deliver ([(0,0)], [(0,0)]) input
    in (length . nub) $ union santa robo

deliver :: Coords -> [Char] -> Coords
deliver (((sx,sy):sacc), ((rx,ry):racc)) (s:r:[]) =
    ((next (sx,sy) s):(sx,sy):sacc, (next (rx,ry) r):(rx,ry):racc)
deliver (s0, r0) (s:r:xs) =
    let (s1, r1) = deliver (s0,r0) (s:r:[])
    in deliver (s1,r1) xs

next :: (Int, Int) -> Char -> (Int, Int)
next (x,y) c =
    case c of
        '>' -> (x+1, y)
        '<' -> (x-1, y)
        '^' -> (x, y-1)
        'v' -> (x, y+1)

