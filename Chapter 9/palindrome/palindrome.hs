main = interact respondPalindrome

respondPalindrome :: String -> String
respondPalindrome = 
    unlines .
    map (\xs -> if isPal xs then "palindrome" else "not a palindrome") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs