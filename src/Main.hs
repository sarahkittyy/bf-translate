module Main where
    
import System.IO
import Data.Char

-- | Returns the factor pairs of an Int
factorPairs :: Int -> [(Int, Int)]
factorPairs n = factorPairs' n 1
    where
        factorPairs' :: Int -> Int -> [(Int, Int)]
        factorPairs' n base
            | fromIntegral base > (sqrt $ fromIntegral n) = []
            | n `mod` base == 0 = (base, n `div` base):(factorPairs' n $ base + 1)
            | otherwise = factorPairs' n $ base + 1

-- | Returns the pair of numbers with the smallest distance between them
closest :: (Num a, Ord a) => [(a, a)] -> (a, a)
closest [] = error $ "No closest pair in an empty list"
closest [a] = a
closest (x:xs) = smaller x (closest xs)
    where
        smaller :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a)
        smaller (a1, b1) (a2, b2) = if abs (b1 - a1) < abs (b2 - a2)
                                        then (a1, b1)
                                        else (a2, b2)

{-| Gets the closest factor pair of a number. If it does not have any factors, it returns the nearest number with factor pairs, as well as the difference between the numbers |-}
nearbyPairing :: Int -> (Int, Int, Int)
nearbyPairing n
    | prime n = let (fx, fy) = closest $ factorPairs (n - 1)
                in (fx, fy, 1)
    | otherwise = let (fx, fy) = closest $ factorPairs n
                  in (fx, fy, 0)
    where
        prime :: Int -> Bool
        prime n = all (/=0) [n `mod` m | m <- [2..n - 1]]

-- | Converts a character into a brainfuck text string
convertChar :: Char -> String
convertChar = (++".[-]") . genNumber . nearbyPairing . ord
    where
        genNumber :: (Int, Int, Int) -> String
        genNumber (f1, f2, r) = (replicate f1 '+') ++ "[>" ++ (replicate f2 '+') ++ "<-]>" ++ (replicate r '+')
        
-- | Converts the given string to brainfuck code.
toBrainfuck :: String -> String
toBrainfuck = concatMap convertChar

-- | Entry point
main :: IO ()
main = getContents >>= (putStr . toBrainfuck)