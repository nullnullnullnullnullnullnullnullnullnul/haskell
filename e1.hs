import Data.Char (isSpace)
{-
  1. Develop a function that receives three positive integers and returns the
  largest of the three, only if it is unique (i.e., the strict maximum). Return -1 if
  there is none. Do not use logical operators (and, or, not). Also develop
  a program to enter the three values, call the function, and display
  the maximum found, or an informational message if it does not exist.
-}

input :: String -> IO Int
input msg = do
  putStrLn msg
  line <- getLine
  let cleanLine = dropWhile isSpace . reverse . dropWhile isSpace . reverse $ line
  case reads cleanLine :: [(Int, String)] of -- takes a String and tries to convert to Int
    [(n, "")] | n > 0 -> return n -- returns the number if everything was parsed correctly
    _ -> do -- else
      putStrLn "Please insert a valid integer."
      input msg

greaterSt :: Int -> Int -> Int -> Int
greaterSt x y z = 
  let _max = maximum [x, y, z] 
  in if length (filter (== _max)[x, y, z]) > 1 then -1 else _max

main :: IO ()
main = do
  x <- input "Insert the first number: "
  y <- input "Insert the second number: "
  z <- input "Insert the third number: "
  let result = greaterSt x y z
  if result == -1
    then putStrLn "No strict maximum exists."
    else putStrLn $ "The strict maximum is: " ++ show result
