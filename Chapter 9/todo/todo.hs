import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) 
                            [0..] todoTasks
        print $ unlines numberedTasks

remove :: [String] -> IO ()
remove [filename, numberString] = do
    contents <- readFile filename
    let todoTasks = lines contents
    --    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) 
    --                        [0..] todoTasks
    -- These are in text, but don't match the sample output. Commented out
    -- putStrLn "These are your TO-DO items:"
    -- mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile filename
            renameFile tempName filename)

bump :: [String] -> IO ()
bump [filename, numberString] = do
    contents <- readFile filename
    let todoTasks = lines contents
        number = read numberString
        stringToBump = todoTasks !! number
        newTodoItems = unlines $ 
            stringToBump : (delete (todoTasks !! number) todoTasks)
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile filename
            renameFile tempName filename)