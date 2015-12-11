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
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x 