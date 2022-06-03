module Main where

import System.Exit (exitSuccess)

data Task = Task { text :: String
								 , isCompleted :: Bool
								 } deriving (Show)

type Tasks = [Task]

data Result = Result { result :: String
										 , tasks :: Tasks
										 } deriving (Show)

addTask :: Tasks -> String -> Result
addTask tasks text = Result "success" ((Task text False):tasks)

deleteTask :: Tasks -> Int -> Result
deleteTask tasks n
	| n >= 0 && n < length tasks = Result "success" newTasks
	| otherwise = Result "wrong task number" tasks
		where
			left = take n tasks
			right = drop (n+1) tasks
			newTasks = left ++ right

completeTask :: Tasks -> Int -> Result
completeTask tasks n
	| n >= 0 && n < length tasks = Result "success" newTasks
	| otherwise = Result "wrong task number" tasks
		where
			left = take n tasks
			right = drop (n+1) tasks
			task = head $ take (n+1) tasks
			newTasks = left ++ (Task (text task) True):right

processCommand :: Tasks -> String -> Result
processCommand tasks cmd
	| n == 0 = Result "exit" tasks
	| n == 1 = Result (show tasks) tasks
	| n == 2 = addTask tasks inp
	| n == 3 = deleteTask tasks (read inp)
	| n == 4 = completeTask tasks (read inp)
	| otherwise = Result "wrong option" tasks
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
	if result r == "exit" then return () else do
		putStrLn $ result r
		putStrLn ""
		processCommands $ tasks r

main :: IO ()
main = do
	processCommands []
