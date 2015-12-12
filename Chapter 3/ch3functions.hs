first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "Empty lists have no head!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty!"
tell (x:[]) = "The list has one element, " ++ show x
tell (x:y:[]) = "The list has two elements, the first element is " ++ show x ++ " and the second is " ++ show y
tell (x:y:_) = "This is a really long list. It's first and second elements are " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "No letters!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare x y
    | x == y = EQ
    | x > y = GT
    | otherwise = LT

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're still underwieght, eat stuff!"
    | bmi <= normal = "Looking good! You're normal."
    | bmi <= overweight = "Getting a little heavy, you're overweight."
    | otherwise = "Go see a doctor. You're obese."
    where bmi = weight / height^2
          (skinny, normal, overweight) = (18.5, 25.0, 30.0)

greet :: String -> String
greet "Joe" = niceGreeting ++ " Joe!"
greet "Kyle" = niceGreeting ++ " Kyle!"
greet name = badGreeting ++ " " ++ name ++ "."

niceGreeting :: String
niceGreeting = "Oh hey, it's"

badGreeting :: String
badGreeting = "Boo, it's just"

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++"."
    where (f:_, l:_) = (firstName, lastName)

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi weight height | (weight, height) <- xs]
    where bmi weight height = weight / height^2

cylinder :: Double -> Double -> Double
cylinder r h = 
    let side = 2*pi*r*h
        top = pi*r^2
    in side + 2 * top

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w/h^2, bmi > 25.0]

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton."
                                               xs -> "a longer list."



