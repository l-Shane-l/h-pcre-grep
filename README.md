[![progress-banner](https://backend.codecrafters.io/progress/grep/24a432be-bb93-4a4b-9a43-f6d7a369912b)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

# Haskell Grep or more accurate Haskell commandline Pcre

Build this as a educational project and to investigate
Haskells parsing ability as well as suitability
for making command line tools

## Notes

- In the beginning of this task I implemented the logic
  in a naive way using pattern matching to parse all possible inputs
  then I became lazy and instead implemented a pass through for the patterns
  Instead of reinventing the wheel i went searching for a haskell library
  for regex and investigated tdfa and the grep default but was excited to find
  support for PCRE, this is really cool because I can use the same regex
  in all my main languages.

- Im really happy with the outcome, it takes a little bit of time to compile
  but its something I could actually use instead of my default grep and something
  that is probably better with the enhanced patterns.

- I took on this task as a mean to deep dive on something I thought I knew well
  now I find myself with the potential of building a tool I might use myself in
  daily dev.
