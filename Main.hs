module Main where

import Data.List (intercalate)
import Control.Exception
import System.IO

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
tasksToSaveString ts = intercalate "\n" $ map (\t -> (show $ isCompleted t) ++ " " ++ text t) ts

saveStringToTasks :: String -> Tasks
saveStringToTasks s = map (\l -> Task (unwords $ tail $ words l) (read (head $ words l) :: Bool)) (lines s)

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
			task = ts!!n
			newTasks = left ++ (Task (text task) True):right

processCommand :: Tasks -> String -> Result
processCommand ts cmd
	| n == "exit" = Result "exit" ts
	| n == "show" = Result (showTasks ts) ts
	| n == "add" = addTask ts inp
	| n == "delete" = deleteTask ts (read inp)
	| n == "complete" = completeTask ts (read inp)
 	| n == "help" = Result "SHASHLIKI TODO LIST\n\texit - save and exit\n\tshow - task list\n\tadd TEXT - add task\n\tdelete N - delete task\n\tcomplete N - complete task" ts
	| otherwise = Result "'help' for getting help" ts
	where
		args = words cmd
		n = head args
		inp = unwords $ tail args

processCommands :: Tasks -> IO ()
processCommands ts = do
	cmd <- getLine
	let r = processCommand ts cmd
	if result r == "exit" then do
		saveHandle <- openFile saveFilePath WriteMode
		hPutStr saveHandle (tasksToSaveString ts)
		hClose saveHandle
	 	return ()
	else do
		putStrLn $ result r
		putStrLn ""
		processCommands $ tasks r

main :: IO ()
main = do
	putStrLn "'help' for getting help"
	saveHandle <- openFile saveFilePath ReadWriteMode
	saved <- hGetContents' saveHandle
	processCommands $ saveStringToTasks saved
