import Data.Char (isSpace)
import Text.Printf (printf)
{-
Develop a function that receives two positive integers as parameters
and returns the number resulting from concatenating both parameters as the return value.
For example, if it receives 1234 and 567, it should return 1234567. Using cast
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

cast :: Int -> Int -> Int
cast x y = 
  read $ show x ++ show y :: Int

main :: IO ()
main = do
  x <- input "First numbers: "
  y <- input "Second numbers: "
  printf "%d\n" $ cast x y
