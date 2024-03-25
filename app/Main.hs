module Main where

import System.Environment
import System.Exit
import Data.Char (isDigit)

-- Updated to handle single characters and the \d pattern for digits
matchPattern :: String -> String -> Bool
matchPattern pattern input =
  case pattern of
    "\\d" -> any isDigit input   -- Checks for any digit in the input
    [c]   -> c `elem` input      -- Checks if the single character is in the input
    _     -> error $ "Unhandled pattern: " ++ pattern

main :: IO ()
main = do
  args <- getArgs
  if head args /= "-E" || length args < 2
    then do
      putStrLn "Expected first argument to be '-E' and a pattern provided"
      exitFailure
    else do
      let pattern = args !! 1
      input_line <- getLine
      if matchPattern pattern input_line
        then exitSuccess
        else exitFailure

