import Data.Char (isSpace)
import Text.Printf (printf)
{-
  3. A person wants to keep track of the expenses incurred when traveling on the subway
  within a month. Knowing that this means of transport uses a
  decreasing fare scheme (detailed in the table below), we ask you to develop
  a function that receives as a parameter the number of trips made in a
  given month and returns the total amount spent on trips. Also create a program to verify
  the behavior of the function.
  Table:
  1 to 20  -> Check updated price on the internet.
  21 to 30 -> 20% discount
  31 to 40 -> 30% discount
  +40      -> 40% discount
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

calculateTotal :: Int -> Int -> Int 
calculateTotal n total = n * total

calculateDiscount :: Int -> Int -> Int
calculateDiscount total disc = total * disc `div` 100

checkDiscount :: Int -> Int -> Maybe Int
checkDiscount trips price
  | trips <= 20 = Nothing
  | trips <= 30 = Just $ total - calculateDiscount total 20
  | trips <= 40 = Just $ total - calculateDiscount total 30
  | otherwise   = Just $ total - calculateDiscount total 40
  where
    total = calculateTotal trips price

main :: IO ()
main = do
  trips <- input "Insert the number of trips: "
  price <- input "Insert the price per trip: "
  maybe (putStrLn "Check updated price on website.\n")
        (printf "Total cost: %d.\n")
        (checkDiscount trips price)
