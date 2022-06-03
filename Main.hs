module Main where

import Data.List (intercalate)
import Control.Exception

data Task = Task { text :: String
								 , isCompleted :: Bool
								 }

type Tasks = [Task]

data Result = Result { result :: String
										 , tasks :: Tasks
										 } deriving (Show)

instance Show Task where
	show (Task t True) = "[X] " ++ t
	show (Task t False) = "[ ] " ++ t

saveFilePath = "tasks.save"

showTasks :: Tasks -> String
showTasks ts = intercalate "\n" $ map (\(i, t) -> (show i) ++ (show t)) (zip [0..] ts)

tasksToSaveString :: Tasks -> String
tasksToSaveString = undefined

saveStringToTasks :: String -> Tasks
saveStringToTasks = undefined

addTask :: Tasks -> String -> Result
addTask ts text = Result "success" ((Task text False):ts)

deleteTask :: Tasks -> Int -> Result
deleteTask ts n
	| n >= 0 && n < length ts = Result "success" newTasks
	| otherwise = Result "wrong task number" ts
		where
			left = take n ts
			right = drop (n+1) ts
			newTasks = left ++ right

completeTask :: Tasks -> Int -> Result
completeTask ts n
	| n >= 0 && n < length ts = Result "success" newTasks
	| otherwise = Result "wrong task number" ts
		where
			left = take n ts
			right = drop (n+1) ts
			task = head $ take (n+1) ts
			newTasks = left ++ (Task (text task) True):right

processCommand :: Tasks -> String -> Result
processCommand ts cmd
	| n == 0 = Result "exit" ts
	| n == 1 = Result (showTasks ts) ts
	| n == 2 = addTask ts inp
	| n == 3 = deleteTask ts (read inp)
	| n == 4 = completeTask ts (read inp)
	| otherwise = Result "wrong option" ts
	where
		args = words cmd
		n = read $ head args
		inp = unwords $ tail args

processCommands :: Tasks -> IO ()
processCommands ts = do
	putStrLn "SHASHLIKI TODO LIST"
	putStrLn "\t0 - save and exit"
	putStrLn "\t1 - task list"
	putStrLn "\t2 TEXT - add task"
	putStrLn "\t3 N - delete task"
	putStrLn "\t4 N - complete task"
	cmd <- getLine
	let r = processCommand ts cmd
	if result r == "exit" then do
		writeFile saveFilePath (tasksToSaveString ts)
	 	return ()
	else do
		putStrLn $ result r
		putStrLn ""
		processCommands $ tasks r

main :: IO ()
main = do
	saved <- readFile saveFilePath `catch`
		\e -> const (return "") (e :: IOException)
	processCommands $ saveStringToTasks saved
