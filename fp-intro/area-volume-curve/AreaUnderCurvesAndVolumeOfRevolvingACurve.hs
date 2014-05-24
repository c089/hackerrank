import Text.Printf (printf)

term :: Int -> Int -> Double -> Double
term a b x = (fromIntegral a)*x**(fromIntegral b)

terms :: [Int] -> [Int] -> [(Double->Double)]
terms as bs = map (\(a,b) -> term a b) $ zip as bs

termValues :: [Int] -> [Int] -> Double -> [Double]
termValues as bs x = map (\f -> f x) (terms as bs)

f :: [Int] -> [Int] -> Double -> Double
f as bs x = sum $ termValues as bs x

solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r as bs = [area, volume]
    where area = sum $ map (\y -> y * deltaX) ys
          volume = sum $ map (\y -> pi * y^2 * deltaX) ys
          xs = [(fromIntegral l),((fromIntegral l)+deltaX)..(fromIntegral r)]
          ys = map (f as bs) xs
          deltaX = 0.001

main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
