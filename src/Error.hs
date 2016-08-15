module Error where

-- | This is error type for when a variable is used before is defined (free variable)
data UndefinedVar i n = UndefinedVar i n -- ^ i is extra information (location of variable) and n is the name
    deriving (Show, Eq)
