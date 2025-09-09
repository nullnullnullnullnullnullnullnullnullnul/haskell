{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
import Data.Char (isSpace)
import Text.Printf (printf)
import Control.Monad (when, unless)
{-
5. Write lambda functions to:
a. Report whether a number is oblong. A number is said to be oblong when
it can be obtained by multiplying two consecutive natural numbers. For example,
6 is oblong because it results from multiplying 2 * 3.
b. Report whether a number is triangular. A number is defined as triangular if
it can be expressed as the sum of a group of consecutive natural numbers
starting from 1. For example, 10 is a triangular number because it is
obtained by adding 1+2+3+4.
Both lambda functions receive the number to be evaluated as their only parameter and
return True or False. External aids are not allowed.
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

oblong :: Int -> Bool
oblong = \x ->
  any(\k->x==k*(k+1))
    [1..floor(sqrt$fromIntegral x::Double)] 

triangular :: Int -> Bool
triangular = \x ->
  any ((==x).(\k->k*(k+1)`div`2))
    [1..floor(sqrt$2*fromIntegral x::Double)+1]
{-
any((==x) . f) do f then do f==x
fromIntegral takes both Int and Integer while fromInteger takes only Int
sqrt needs Floating (e.g Double)
floor rounds float to integers
-}

main :: IO ()
main = do
  x <- input "Insert the number to check if its oblong or triangular: "
  let isOblong = oblong x
      isTriangular = triangular x
  when isOblong $ printf "%d is oblong.\n" x
  unless isOblong $ printf "%d is not oblong.\n" x
  when isTriangular $ printf "%d is triangular.\n" x
  unless isTriangular $ printf "%d is not triangular.\n" x
