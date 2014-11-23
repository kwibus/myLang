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
data BruijnTerm = BLambda Index Name BruijnTerm
                | BAppl BruijnTerm BruijnTerm
                | Bound Index deriving Show
            --  | Freevar  should not exsist

lam2Bruijn :: LamTerm -> BruijnTerm
lam2Bruijn t = go t 0 M.empty 
  where go (Var n) _ env =  Bound (env M.! n)
        go (Lambda  n t1 ) maxi env = BLambda  maxi n $ go  t1 (maxi+1) (M.insert n maxi env)
        go (Appl t1 t2) maxi env = BAppl (go t1 maxi env)(go t2 maxi env)

bruijn2Lam :: BruijnTerm -> LamTerm
bruijn2Lam t = go t  IM.empty
  where go (Bound i)  env = Var $ env IM.! i
        go (BAppl t1 t2 ) env = Appl (go t1 env)(go t2 env)
        go (BLambda i n t1) env = Lambda n $ go t1 (IM.insert  i n env)
