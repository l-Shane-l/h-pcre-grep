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

- Using a library here to access PCRE functions might go against the spirit
  of the challenge, how ever I struggle to see the value of hand coding the regex's
  Especially as this is code I want to use beyond the challenge.

- From this challenge I really got to dive deep on regex, and finally understand what grep is.
  I thought it was like other command line tools and didnt realise it was an acronym. Im really happy
  I got the chance to experiment with the varies regex standards and feel like I can reliably use pcre now.

- Building a grep clone with 70 lines of custom haskell is really satisfying and should be the base of a lot of
  my future projects.

## Conclusion

Finally this project really highlighted to me something I struggle to put into clear words.
but its something that inspired my focus on haskell, and that is the speed of development that
is possible through abstraction, I can see it in C++ but its even more pronounces in haskell.

This project gives a really clear demonstration, I could have naively implemented each step of the regex
and even would have thought I was making good progress because of the powerful pattern matching in haskell.
If this was a real world case I might have ended up with a massive multi file project, however investing`
time into learning about the applicative args library and then regex in haskell it thought it might be possible
for me even with my limited haskell experience to build a higher kinded approach and make it way more powerful
It was a 5 hour struggle to get it all working and built and I lost motivation a few times
I even thought it might not be doable. So it was incredibly satisfying to get it working and to see it
not only pass tests 4 but all remaining tests up to 15.

I think this project marks a turning point in my confidence with haskell and the first time, I took a similar approach
to what I would have done in C++ my main language, that is find a shortcut.

Im really happy with this progress and excited to see what the next one will bring

## PCRE Cheat Sheet

# Regular Expression Cheat Sheet - PCRE

| Anchor | Description                                                                         | Example              | Valid match              | Invalid                     |
| :----- | :---------------------------------------------------------------------------------- | :------------------- | :----------------------- | --------------------------- |
| ^      | start of string or line                                                             | ^foam                | foam                     | bath foam                   |
| \A     | start of string in any match mode                                                   | \Afoam               | foam                     | bath foam                   |
| $      | end of string or line                                                               | finish$              | finish                   | finnish                     |
| \Z     | end of string, or char before last new line in any match mode                       | finish\Z             | finish                   | finnish                     |
| \z     | end of string, in any match mode.                                                   |
| \G     | end of the previous match or the start of the string for the first match            | \^(get\|set)\|\G\w+$ | setValue                 | seValue                     |
| \b     | word boundary; position between a word character (\w), and a nonword character (\W) | \bis\b               | This island is beautiful | This island isn't beautiful |
| \B     | not-word-boundary.                                                                  | \Bland               | island                   | peninsula                   |

| Assertion | Description          | Example       | Valid match | Invalid     |
| :-------- | :------------------- | :------------ | :---------- | ----------- |
| (?=...)   | positive lookahead   | question(?=s) | questions   | question    |
| (?!...)   | negative lookahead   | answer(?!s)   | answer      | answers     |
| (?<=...)  | positive look-behind | (?<=appl)e    | apple       | application |
| (?<!...)  | negative look-behind | (?<!goo)d     | mood        | good        |

| Char class | Description                     | Example       | Valid match    | Invalid      |
| :--------- | :------------------------------ | :------------ | :------------- | ------------ |
| [ ]        | class definition                | [axf]         | a, x, f        | b            |
| [ - ]      | class definition range          | [a-c]         | a, b, c        | d            |
| [ \ ]      | escape inside class             | [a-f\.]       | a, b, .        | g            |
| [^ ]       | Not in class                    | [^abc]        | d, e           | a            |
| [:class:]  | POSIX class                     | [:alpha:]     | string         | 0101         |
| .          | match any chars except new line | b.ttle        | battle, bottle | bttle        |
| \s         | white space, [\n\r\f\t ]        | good\smorning | good morning   | good.morning |
| \S         | no-white space, [^\n\r\f\t]     | good\Smorning | good.morning   | good morning |
| \d         | digit                           | \d{2}         | 23             | 1a           |
| \D         | non-digit                       | \D{3}         | foo, bar       | fo1          |
| \w         | word, [a-z-A-Z0-9_]             | \w{4}         | v411           | v4.1         |
| \W         | non word, [^a-z-A-Z0-9_]        | .$%?          | .$%?           | .ab?         |

| Special character | Description                                |
| :---------------- | :----------------------------------------- |
| \\                | general escape                             |
| \n                | new line                                   |
| \r                | carriage return                            |
| \t                | tab                                        |
| \v                | vertical tab                               |
| \f                | form feed                                  |
| \a                | alarm                                      |
| [\b]              | backspace                                  |
| \e                | escape                                     |
| \cchar            | Ctrl + char(ie:\cc is Ctrl+c)              |
| \ooo              | three digit octal (ie: \123)               |
| \xhh              | one or two digit hexadecimal (ie: \x10)    |
| \x{hex}           | any hexadecimal code (ie: \x{1234})        |
| \p{xx}            | char with unicode property (ie: \p{Arabic} |
| \P{xx}            | char without unicode property              |

| Pattern modifier | Description                                                     |
| :--------------- | :-------------------------------------------------------------- |
| g                | global match                                                    |
| i                | case-insensitiv, match both uppercase and lowercase             |
| m                | multiple lines                                                  |
| s                | single line (by default)                                        |
| x                | ingore whitespace allows comments                               |
| A                | anchored, the pattern is forced to ^                            |
| D                | dollar end only, a dollar metacharacter matches only at the end |
| S                | extra analysis performed, useful for non-anchored patterns      |
| U                | ungreedy, greedy patterns becomes lazy by default               |
| X                | additional functionality of PCRE (PCRE extra)                   |
| J                | allow duplicate names for subpatterns                           |
| u                | unicode, pattern and subject strings are treated as UTF-8       |
