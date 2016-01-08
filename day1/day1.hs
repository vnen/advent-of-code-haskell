import System.IO

main :: IO ()
main = do
    input <- readFile "input1.txt"
    putStrLn $ show (count input)
    

count :: String -> Int
count = foldl (\acc c -> if c == '(' then acc + 1 else acc - 1) 0
