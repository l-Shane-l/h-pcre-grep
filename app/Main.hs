module Main where

import Data.Char (isAlpha, isDigit)
import System.Environment
import System.Exit

-- Updated to handle single characters and the \d pattern for digits
matchPattern :: String -> String -> Bool
matchPattern pattern input =
  case pattern of
    "\\w" -> all isAlpha input
    "\\d" -> any isDigit input -- Checks for any digit in the input
    [c] -> c `elem` input -- Checks if the single character is in the input
    ('[' : cs) ->
      if head cs == '^'
        then not $ any (`elem` input) (tail $ init cs) -- match first "[" then use init to remove ending "]"
        else (`elem` input) `any` init cs
    _ -> error $ "Unhandled pattern: " ++ pattern

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
