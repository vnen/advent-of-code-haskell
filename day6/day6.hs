module Main where
--import           Data.List

main :: IO ()
main = do
  input <- readFile "input6.txt"
  print $ lightsOn (runGrid input initialGrid)
  --putStrLn $ unlines (map (show . parseLine) (lines input))

data Coord = Coord Int Int
  deriving(Eq, Show)

data Light = Off | On
  deriving(Eq, Ord, Show)

-- What to do, start, end
data Line = Line (Light -> Light) Coord Coord

type GridLight = (Coord, Light)

initialGrid :: [GridLight]
initialGrid = [(Coord x y, Off) | x <- [0..999], y <- [0..999]]

lightsOn :: [GridLight] -> Int
lightsOn []            = 0
lightsOn ((_, On):ls)  = 1 + lightsOn ls
lightsOn ((_, Off):ls) = lightsOn ls

toggle :: Light -> Light
toggle On  = Off
toggle Off = On

turnOn :: Light ->Light
turnOn _ = On

turnOff :: Light ->Light
turnOff _ = Off

toCoord :: String -> Coord
toCoord s =
  Coord (read x) (read y)
  where
    x = takeWhile (/=',') s
    y = tail $ dropWhile (/=',') s

-- Apply a function to the range of coordinates (toggle, on or off)
applyRange :: (Light -> Light) -> Coord -> Coord -> [GridLight] -> [GridLight]
applyRange _ _ _ [] = []
applyRange f (Coord x1 y1) (Coord x2 y2) ((Coord x y, l):ls)
  | x > x2 && y > y2 = (Coord x y, l):ls
  | x1 <= x && x <= x2 && y1 <= y && y <= y2 =
    (Coord x y, f l):applyRange f (Coord x1 y1) (Coord x2 y2) ls
applyRange f start end (l:ls) = l:applyRange f start end ls

parseLine :: String -> Line
parseLine s =
  Line action (toCoord start) (toCoord end)
  where
    ps = words s
    action = case head ps of
      "toggle" -> toggle
      "turn"   -> if ps !! 1 == "on" then turnOn else turnOff
      _        -> undefined
    start = if head ps == "toggle" then ps !! 1 else ps !! 2
    end = if head ps == "toggle" then ps !! 3 else ps !! 4

runLine :: Line -> [GridLight] -> [GridLight]
runLine (Line f s e)  = applyRange f s e

runGrid :: String -> [GridLight] -> [GridLight]
runGrid i g =
  let actions = map parseLine $ lines i
  in
    foldl (flip runLine) g actions
