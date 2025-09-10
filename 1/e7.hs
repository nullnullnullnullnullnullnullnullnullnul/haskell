import           Data.Char   (isDigit, isSpace)
import           Text.Printf (printf)

{-
7. Write a function called nextday(day, month, year) that receives as a parameter any
date expressed by three integers and calculates and returns three other integers
corresponding to the day following the given date. Using this function without modifications
or additions, develop programs that allow you to:
a. Add N days to a date.
b. Calculate the number of days between any two dates.
-}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim xs =
  case break (== delim) xs of
    (chunk, [])     -> [chunk]
    (chunk, _:rest) -> chunk : splitOn delim rest

checkDateFormat :: String -> Bool
checkDateFormat date =
  case splitOn '/' date of
    parts
      | length parts == 3 ->
      (not . any null) parts &&                      -- no empty
      all (all isDigit) parts &&                     -- all integers
      let nums = map read parts :: [Int] in
      all (>0) nums                                  -- all positive
    _ -> False

isLeap :: Int -> Bool
isLeap year =
  year `mod` 4 == 0 && year `mod` 100 /= 0 || year `mod` 400 /= 0

daysInMonth :: Int -> Int -> Int
daysInMonth m y
  | m == 2                     = if isLeap y then 29 else 28
  | m `elem` [1,3,5,7,8,10,12] = 31
  | m `elem` [4,6,9,11]        = 30
  | otherwise                  = 0

checkDate :: Int -> Int -> Int -> Bool
checkDate d m y = not $ d > 31 || m > 12 || d > daysInMonth m y

input :: String -> IO String
input msg = do
  printf msg
  line <- getLine
  let cleanLine = dropWhile isSpace . reverse . dropWhile isSpace . reverse $ line
  return cleanLine

nextDay :: Int -> Int -> Int -> (Int, Int, Int)
nextDay d m y
  | d < daysInMonth m y = (d+1, m, y)
  | m < 12              = (1, m+1, y)
  | m == 12             = (1, 1, y+1)
  | otherwise           = (d, m , y)

main :: IO ()
main = do
  dateStr <- input "Insert the date(dd/mm/yyyy): "
  if checkDateFormat dateStr
    then case map read $ splitOn '/' dateStr :: [Int] of
      [d,m,y] ->
        if checkDate d m y
          then do
            let (nd, nm, ny) = nextDay d m y
            printf "Next day is %d/%d/%d" nd nm ny
          else do
            putStrLn "Invalid date."
            main
      _ -> main
    else do
      putStrLn "Invalid format, please use: dd/mm/yyyy"
      main
