import System.Random
import System.IO
import System.Environment

main = do
    (numString:outFile:_) <- getArgs
    gen <- getStdGen
    let num = (read numString) * 3
        rands = take num (randomRs (1,100) gen) :: [Int]
        fileString = unlines $ map show rands
    writeFile outFile fileString

