import Data.Char (isSpace)
import Text.Printf (printf)
import qualified Data.IntMap.Strict as IM
import Control.Monad (foldM, forM_, when)
import Control.Monad.State
{-
4. An appliance store needs a program for its checkout line that
tells the cashier how much change to give the customer. To do this, two integers are entered,
corresponding to the total purchase and the money received.
Report how many bills of each denomination should be given as change,
in such a way as to minimize the number of bills. Consider that there are bills
of $5000, $1000, $500, $200, $100, $50, and $10. Issue an error message if the
money received is insufficient or if the change cannot be given due to a lack
of bills with the appropriate denominations. Example: If the purchase is $3170 and is
paid with $5000, the change should contain 1 $1000 bill, 1 $500 bill, 1
$200 bill, 1 $100 bill, and 3 $10 bills.
-}
type Cash = IM.IntMap Int
keys :: [Int]
keys = [5000, 1000, 500, 200, 100, 50, 10]
initCash :: IM.IntMap Int
initCash = IM.fromList [(k, 0) | k <- keys]

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

enoughCash :: Int -> Int -> Bool
enoughCash total cash = cash >= total

getCash :: IO (Int, Int) -- cash, total
getCash = do
  cash <- input "Insert the cash received: "
  total <- input "Insert the total purchase amount: "
  if enoughCash total cash
    then return (cash, total)
    else do 
      printf "The cash is not enough for the purchase.\n"
      getCash

depositCash :: Int -> Int -> State Cash ()
depositCash k n = modify (IM.adjust (+n) k)

calculateChange :: Int -> Int -> State Cash Int
calculateChange total received = do
  let change = received - total
  if change == 0
    then return 0
    else do foldM giveBills change keys
  where 
    giveBills :: Int -> Int -> State Cash Int
    giveBills re k = do
      let n = re `div` k
      depositCash k n
      return (re `mod` k)

printBills :: Cash -> IO ()
printBills cash = do
  forM_ (IM.assocs cash) $ \(k,v) ->
    when (v > 0) $
    putStrLn $ show v ++ " bills of $" ++ show k
{-
forM_ -> iterates through the tuple, "_" -> doesnt care about returned values
IM.assocs cash -> casts IntMap dict to tuple (key, value) 
-}

main :: IO ()
main = do
  (received, total) <- getCash
  let (finalRemainder, updatedCash) = runState (calculateChange total received) initCash
  if finalRemainder /= 0
    then putStrLn "Insufficient bills."
    else printBills updatedCash
