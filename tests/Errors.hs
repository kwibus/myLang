module Errors where


import Interperter
main = mapM readEvalPrint errorStrings
errorStrings :: [String]
errorStrings =
    [ "\\a.a a"
    , "1.0 2.0"
    , "(\\a.a) 1.0 2.0"
    , "(\\a.a) a 1.0 a"
    , "\\a.\\f. (f a) ( f a 1.0)"
    ]
