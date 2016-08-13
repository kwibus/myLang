module Lam1 where

import Info
import Lambda
import Name

type Lam1 =LamTerm Pattern SourcePos Name

data Pattern = Pattern {location :: SourcePos, patName::Name} deriving (Show,Eq)

instance HasName Pattern where
    getName = patName
    setName p n =  p{patName =n}

instance HasPostion Pattern where
    getPosition = location
