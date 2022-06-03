module Main where

import System.Exit (exitSuccess)

data Tasks = Tasks [String]


processCommand :: Integer -> IO ()
processCommand 0 = exitSuccess
processCommand _ = putStrLn "notheing"

getInt :: IO Integer
getInt = readLn

main :: IO ()
main = do
	inp <- getInt
	processCommand inp
	main
