module Type where

import Data.Coerce
import Enviroment
import Control.Monad.State

-- TODO move location
bound2Free :: Type Bound -> Type Free
bound2Free = coerce

data MonoType = TDouble deriving (Eq, Show)
data Type i = TVal MonoType
            | TVar i
            | TAppl (Type i) (Type i) deriving (Eq, Show)

{-
 data Type = TVal MonoType | TVar Key | TAppl Type Type deriving Eq
 instance Show Type where
     show =tShow
-}

tSize :: Type i -> Int
tSize (TVal {}) = 1
tSize (TVar {}) = 1
tSize (TAppl t1 t2) = tSize t1 + tSize t2

tShow :: Type Bound -> String
tShow t = evalState (go t) (bEmtyEnv , ['a' .. 'z'])
    where go :: Type Bound -> State (BruiEnv String , [Char]) String
          go (TVal v) = return (pShowType v)
          go (TAppl t1 t2) = do
            s1 <- go t1
            s2 <- go t2
            let s1' = case t1 of
                 TAppl {} -> "(" ++ s1 ++ ")"
                 _ -> s1
            return $ s1' ++ " -> " ++ s2
          go (TVar i ) = do
            (m, names) <- get
            if bMember i m
                then return ( bLookup i m)
                else let newname = [head names]
                         newMap = binsertAt newname i m
                     in put (newMap, (tail names) ) >> return newname

pShowType :: MonoType -> String
pShowType TDouble = "Double"
