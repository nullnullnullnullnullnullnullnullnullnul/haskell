import Data.Char (isSpace)
import Text.Printf (printf)
{-
2. Develop a function that receives three positive integers corresponding to the day,
month, and year of a date and verifies whether they correspond to a valid date. The
number of days in each month must be taken into account, including leap years. Return
True or False depending on whether the date is correct or not. Also, create a program
to verify the behavior of the function.
-}

input :: String -> IO Int
input msg = do
  printf msg
  line <- getLine
  let cleanLine = dropWhile isSpace . reverse . dropWhile isSpace . reverse $ line
  case reads cleanLine :: [(Int, String)] of
    [(n, "")] | n > 0 -> return n
    _ -> do
      printf "Please insert a valid integer."
      input msg

leapYear :: Int -> Bool
leapYear year
  | year `mod` 400 == 0 = True -- backticks(``) between "mod" allow to write it like that
  | year `mod` 100 == 0 = False -- without backticks this line would be: mod year 100 == 0 = False
  | year `mod`   4 == 0 = True
  | otherwise           = False

daysInMonth :: Int -> Int -> Int
daysInMonth mm year
  | mm == 2              = if leapYear year then 29 else 28
  | mm `elem` [4,6,9,11] = 30 -- elem mm [4,6,9,11]
  | otherwise            = 31

validDate :: Int -> Int -> Int -> Bool
validDate dd mm year
  | dd <= 0 || mm <= 0 || year < 0 = False
  | mm > 12                        = False
  | dd > daysInMonth mm year       = False
  | otherwise                      = True

main :: IO ()
main = do
  dd   <- input "Day: "
  mm   <- input "Month: "
  year <- input "Year: "
  if validDate dd mm year
    then printf "%04d/%02d/%02d is a valid date.\n" year mm dd
    else printf "The date %d/%d/%d is invalid.\n" year mm dd
