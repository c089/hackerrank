module Main where

fib n = fib' n 0 1

fib' 1 f1 f2 = f1
fib' n f1 f2 = fib' (n-1) f2 (f1+f2)

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ input

