module Main
where

import Parser

main :: IO()
main = print $ parseString  "\\a.a"
