module TestUtils where
import Data.Char

removeNewLines :: String -> String
removeNewLines str = concatMap (dropWhile isSpace) $ lines str
