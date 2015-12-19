import Data.List (tails, isPrefixOf, any)

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) $ (tails haystack)