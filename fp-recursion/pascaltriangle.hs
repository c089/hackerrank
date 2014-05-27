factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

val :: Int -> Int -> Int
val n r = factorial n `div` (factorial r * factorial (n-r))

vals :: Int -> [Int]
vals n = [val n c | c <- [0..n] ]

line :: Int -> String
line n = unwords $ map show $ vals n

main = do
    input  <- getLine
    let n = (read :: String -> Int) input
    mapM_ putStrLn $ map line [0..(n-1)]
