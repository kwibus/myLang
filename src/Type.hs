module Type where

import qualified Data.IntMap as IM

import Names
import Data.Coerce
import Enviroment
import Control.Monad.State

-- TODO move location
typeBound2Free :: Type Bound -> Type Free
typeBound2Free = coerce

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

tShow :: ToInt i => Type i -> String
tShow t = evalState (tShowEnv t) initState

tShowEnv :: ToInt i => Type i -> State (IM.IntMap String , [String]) String
tShowEnv (TVal v) = return (pShowType v)
tShowEnv (TAppl t1 t2) = do
            s1 <- tShowEnv t1
            s2 <- tShowEnv t2
            let s1' = case t1 of
                 TAppl {} -> "(" ++ s1 ++ ")"
                 _ -> s1
            return $ s1' ++ " -> " ++ s2
tShowEnv (TVar i ) = do
            (m, names) <- get
            case IM.lookup (toInt i) m of
                Just str -> return str
                Nothing ->
                    let newname = head names
                        newMap = IM.insert (toInt i) newname m
                    in put (newMap, (tail names) ) >> return newname

initState :: (IM.IntMap String, [String] )
initState = (IM.empty , letters)

genNames :: ToInt i => Type i -> State (IM.IntMap String , [String]) ()
genNames (TVal _) = return ()
genNames (TVar i) = do
    (m , names) <- get
    put (IM.insert (toInt i) (head names) m, init names)
genNames (TAppl t1 t2) = genNames t1 >> genNames t2

pShowType :: MonoType -> String
pShowType TDouble = "Double"
