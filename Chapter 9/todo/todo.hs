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
dispatch command = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = 
    putStrLn $ "The " ++ command ++ " command doesn't exist."

main = do
    contents <- getArgs
    if length contents >= 2
        then dispatch (head contents) (tail contents)
        else putStrLn "A command and arguments must be supplied."

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments."

view :: [String] -> IO ()
view [fileName] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) 
                            [0..] todoTasks
        print $ unlines numberedTasks
view _ = putStrLn "The view command takes exactly one arguments."

remove :: [String] -> IO ()
remove [filename, numberString] = do
    contents <- readFile filename
    let todoTasks = lines contents
    -- These are in text, but don't match the sample output. 
    -- Commented out to match.
    --    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) 
    --                        [0..] todoTasks
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
remove _ = putStrLn "The remove command takes exactly two arguments."

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
bump _ = putStrLn "The bump command takes exactly two arguments."