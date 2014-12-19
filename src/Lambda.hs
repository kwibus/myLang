-- {-# LANGUAGE  GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Lambda where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Monad.State.Strict

type Name = String

data LamTerm = Lambda Name LamTerm
            | Appl LamTerm LamTerm
            | Var Variable
            deriving (Eq, Show)

var :: Name -> LamTerm
var = Var . VarVar

val :: Vallue -> LamTerm
val = Var . Val

-- TODO rename VarVar
data Variable = VarVar Name | Val Vallue deriving (Eq, Show )
data Vallue = MyDouble Double
    |BuildIn { name :: Name
             , arrity :: Int
             , evaluator :: State Stack Vallue
             , stack :: Stack}

type Stack = [Vallue]

push :: Vallue -> State  Stack ()
push v = modify (v:)
data Type = TDouble|TBool

pop ::State Stack Vallue
pop = do
    s <- get
    put $ tail s
    return $ head s


plus :: Vallue
plus = BuildIn {name ="plus"
               ,arrity = 2
               ,evaluator = evalPlus
               ,stack = []
               }
evalPlus :: State Stack Vallue
evalPlus = do 
    MyDouble a <- pop
    MyDouble b <- pop 
    return $ MyDouble $ a + b


instance Show Vallue where
    show (MyDouble a) = show a
    show BuildIn {name = n} = n

instance Eq Vallue where
    (==) (MyDouble a) (MyDouble b) = abs (a - b) < 0.0001

pShowVar :: Variable -> String
pShowVar (VarVar n) = n
pShowVar (Val v) = pShowVal v

pShowVal :: Vallue -> String
pShowVal (MyDouble a) = show a

pShowVal BuildIn {name = n} = n

pShow :: LamTerm -> String
pShow = go False where
      go _ (Var n) = pShowVar n
      go b (Lambda n t) = "\\" ++ n ++ "." ++ go b t
      go b (Appl t1@Lambda{} t2@Var {}) = parentheses t1 ++ go b t2
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
eval (BAppl (BLambda _ t) t2) = Just $ substitute t2 0 t
eval (BAppl (BVar (BVal (t@BuildIn{}))) (BVar (BVal v  ))) = Just $ bval $ apply t v
eval (BAppl t1@BAppl{} t2@BVar{}) =  fmap (\t -> BAppl t t2 ) $ eval t1
eval (BAppl t1@BAppl{} t2) =  fmap (BAppl t1 ) $eval t2
eval (BAppl t1 t2) = fmap (\t -> BAppl t t2 ) $ eval t1

apply :: Vallue -> Vallue -> Vallue
apply   BuildIn{arrity = 1,evaluator = e,stack = s  } v = evalState e (v:s )
apply t@BuildIn{arrity = n,stack = s  } v =t {arrity = n - 1 , stack = v:s} 
apply _ _= error "apply vallue"

--Todo remove inita index
substitute ::  BruijnTerm -> Index -> BruijnTerm -> BruijnTerm
substitute t1 i1 t2@(BVar (Bound i2)) = if i1 == i2 then t1 else t2
substitute t1 i1 (BLambda n t2) = BLambda n $ substitute t1 (i1+1) t2
substitute t i (BAppl t1 t2) = BAppl (substitute t i t1) (substitute t i t2)
substitute _ _  t2 = t2

isvalue :: BruijnTerm -> Bool
isvalue BVar {} = True
isvalue BAppl {} = False
isvalue BLambda {} = True

