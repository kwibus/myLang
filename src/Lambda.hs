{-# LANGUAGE  GADTs #-}
module Lambda where

import qualified Data.IntMap as IM
import qualified Data.Map as M


type Name = String

data LamTerm = Lambda Name LamTerm
            | Appl LamTerm LamTerm 
            | Var Variable 
            deriving (Eq,Show)

var ::Name -> LamTerm 
var = Var . VarVar

val ::Vallue -> LamTerm 
val = Var . Val

data Variable  = VarVar Name | Val Vallue deriving (Eq )
data Vallue  = MyDouble Double 

-- instance Show (Vallue a) where
--     show (MyDouble d)  = show d
instance Show Variable  where
    show (VarVar n) = n
    show (Val v) =  show v

instance Show Vallue where
    show (MyDouble a) = show a

instance Eq Vallue  where
    (MyDouble a ) == (MyDouble b) = a == b

pShow ::  LamTerm  -> String
pShow = go False where
      go _ (Var n) =show n
      go b (Lambda n t) = "\\" ++ n ++"." ++ go b t
      go b (Appl t1@Lambda{} t2@Var{}) = parentheses t1 ++ go b t2
      go _ (Appl t1@Lambda{} t2) = parentheses t1 ++ parentheses t2
      go b (Appl t1@Var {} t2@Var{} )= go b t1 ++" "++ go b t2
      go b (Appl t1@Var {} t2 )= go b t1 ++ parentheses t2
      go _ (Appl t1@Appl {} t2@Appl{} )= go True t1 ++ parentheses t2
      go True (Appl t1@Appl {} t2@Lambda{})= go True t1 ++ parentheses t2
      go b (Appl t1@Appl {} t2@Var{} )= go True t1 ++" "++ go b t2
      go b (Appl t1@Appl {} t2 )= go True t1 ++ go b t2


parentheses :: LamTerm -> String
parentheses s = "(" ++ pShow s ++ ")"

type Index = Int
data BruijnTerm = BLambda Name BruijnTerm 
                | BAppl BruijnTerm  BruijnTerm
                | BVar Bvariable deriving (Eq,Show)
            --  | Freevar  should not exsist

bvar ::Int -> BruijnTerm
bvar = BVar . Bound

bval ::Vallue -> BruijnTerm
bval = BVar . BVal

data Bvariable = Bound Index | BVal Vallue deriving (Show, Eq)

lam2Bruijn :: LamTerm -> BruijnTerm
lam2Bruijn t = go t 0 M.empty
  where go (Var (VarVar n)) depth  env =  bvar (depth - (env M.! n)-1)
        go (Var (Val v)) _ _ = bval v
        go (Lambda  n t1) depth env = BLambda  n $ go  t1 (depth +1) (M.insert n depth env)
        go (Appl t1 t2) depth  env = BAppl (go t1 depth env)(go t2 depth env)

bruijn2Lam :: BruijnTerm -> LamTerm
bruijn2Lam t = go t 0 IM.empty
  where go (BVar (Bound i)) depth env = var $ env IM.!  (depth - i -1 )
        go (BVar (BVal v)) _ _ = val  v
        go (BAppl t1 t2 ) depth env = Appl (go t1 depth env)(go t2 depth env)
        go (BLambda  n t1) depth env = Lambda n $ go t1 (depth +1)(IM.insert depth n env)

eval :: BruijnTerm -> Maybe BruijnTerm 
eval (BVar {}) = Nothing
eval (BLambda {}) = Nothing --fmap (BLambda n ) $ eval t
eval (BAppl (BLambda _ t) t2) = Just $ substitute t 0 t2
eval (BAppl t1 t2 )
    | isvalue t1  = fmap (\t -> BAppl t t1 ) $ eval  t2
    | otherwise = fmap (BAppl t1 ) $eval t2

substitute ::  BruijnTerm -> Index -> BruijnTerm -> BruijnTerm
substitute t1 i1 t2@(BVar (Bound i2)) = if i1 == i2 then t1 else t2
substitute t1 i1 (BLambda n t2) = BLambda n $ substitute t1 (i1+1) t2
substitute t i (BAppl t1 t2) = BAppl (substitute t i t1) (substitute t i t2)
substitute _ _  t2 = t2

isvalue :: BruijnTerm -> Bool
isvalue BVar {} = True
isvalue BAppl {} = False
isvalue BLambda {} = True
