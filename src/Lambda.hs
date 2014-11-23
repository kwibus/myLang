module Lambda where

import qualified Data.IntMap as IM
import qualified Data.Map as M

type Name = String

data LamTerm= Lambda Name LamTerm
            | Appl LamTerm LamTerm
            | Var Name
            deriving Eq
instance Show LamTerm where
    show (Var n) = n
    show (Lambda n t) = "\\" ++ n ++ "." ++ show t
    show (Appl t1@ Lambda {} t2)  = parentheses t1 ++ show t2
    show (Appl t1@ Var {}    t2@ Appl  {}) = show t1 ++ parentheses t2
    show (Appl t1@ Var {}    t2) = show t1 ++ show t2
    show (Appl t1@ Appl {}   t2) = show t1 ++ show t2

parentheses :: Show a => a -> String
parentheses s = "(" ++ show s ++ ")"

type Index = Int
data BruijnTerm = BLambda Name BruijnTerm
                | BAppl BruijnTerm BruijnTerm
                | Bound Index deriving (Show, Eq)
            --  | Freevar  should not exsist

lam2Bruijn :: LamTerm -> BruijnTerm
lam2Bruijn t = go t 0 M.empty 
  where go (Var n) depth  env =  Bound (depth - (env M.! n)-1)
        go (Lambda  n t1) depth env = BLambda  n $ go  t1 (depth +1) (M.insert n depth env)
        go (Appl t1 t2) depth  env = BAppl (go t1 depth env)(go t2 depth env)

bruijn2Lam :: BruijnTerm -> LamTerm
bruijn2Lam t = go t 0 IM.empty
  where go (Bound i) depth env = Var $ env IM.!  (depth - i -1 )
        go (BAppl t1 t2 ) depth env = Appl (go t1 depth env)(go t2 depth env)
        go (BLambda  n t1) depth env = Lambda n $ go t1 (depth +1)(IM.insert depth n env)

eval :: BruijnTerm -> Maybe BruijnTerm
eval (Bound {}) = Nothing
eval (BLambda {}) = Nothing --fmap (BLambda n ) $ eval t
eval (BAppl (BLambda _ t) t2) = Just $ substitute t 0 t2 
eval (BAppl t1 t2 ) 
    | isvalue t1  = fmap (\t -> BAppl t t1 ) $ eval  t2
    | otherwise = fmap (BAppl t1 ) $eval t2

substitute ::  BruijnTerm -> Index -> BruijnTerm -> BruijnTerm 
substitute t1 i1 t2@(Bound i2) = if i1 == i2 then t1 else t2
substitute t1 i1 (BLambda n t2) = BLambda n $ substitute t1 (i1+1) t2
substitute t i (BAppl t1 t2) = BAppl (substitute t i t1) (substitute t i t2)

isvalue :: BruijnTerm -> Bool
isvalue Bound {} = True
isvalue BAppl {} = False
isvalue BLambda {} = True
