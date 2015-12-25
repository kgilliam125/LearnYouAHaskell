import Control.Monad
import Data.Char

main = forever $ do
    l <- getLine
    putStr $ map toUpper l