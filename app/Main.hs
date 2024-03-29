{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import Text.Regex.PCRE

-- create a ADT for code reability
data Options = Options
  { regexEnabled :: Bool,
    regexPattern :: String,
    inputLine :: Maybe String
  }

-- use optParse to clean up code and provide nice output to users
optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch
      ( long "regex"
          <> short 'E'
          <> help "Enable regex pattern matching"
      )
    <*> argument
      str
      ( metavar "PATTERN"
          <> help "Regex pattern to match against the input"
      )
    <*> optional
      ( strOption
          ( long "input"
              <> short 'i'
              <> metavar "INPUT"
              <> help "Input line to search"
          )
      )

parserInfo :: ParserInfo Options
parserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Matches a regex PATTERN against an INPUT line"
        <> header "haskell-regex - a command-line regex matcher"
    )

-- simple function to make use of Haskells PCRE
matchPattern :: String -> String -> Bool
matchPattern pattern input = input =~ pattern

main :: IO ()
main = do
  opts <- execParser parserInfo
  let pattern = regexPattern opts
  let enableRegex = regexEnabled opts
  input <- maybe getLine return (inputLine opts)

  if not enableRegex
    then do
      putStrLn "Regex pattern matching not enabled. Use -E to enable."
      exitFailure
    else
      if matchPattern pattern input
        then do
          putStrLn "Pattern matches the input."
          exitSuccess
        else do
          putStrLn "No match found."
          exitFailure
