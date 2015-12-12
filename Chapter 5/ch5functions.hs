multThree :: (Num a) => a -> a -> a -> a
multThree a b c = a*b*c

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a 
divideByTen = (/10)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter (<=x) xs
        greater = filter (>x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort greater

largestDivisible :: Integer
largestDivisible = head (filter p [99999,99998..])
    where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n * 3 + 1 )

numChains :: Int
numChains = length ( filter (\xs -> length xs > 15) ( map (chain) [1..100] ))

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = \x y -> f y x 
