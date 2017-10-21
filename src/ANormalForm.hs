{-# LANGUAGE MonoLocalBinds#-}
module ANormalForm
  (module ANormalForm
  ,Name)
where

import Data.Bifunctor

import qualified Value as Lit --TODO rename Value Literal/Constant
import Value (BuildIn,Primative)
import qualified BruijnTerm as Lam
import BruijnTerm (BruijnTerm,Bound(..),fullApplied)
import Name

import qualified Unprocessed
import Unprocessed hiding (accumulateVars)
import LambdaF

--TODO consitent names with value
data Value = Var Bound
           | Constant Primative
           | Instruc BuildIn -- TODO maybe remove
           | Lambda  Name ANorm -- TODO mak name optional?
           deriving (Show ,Eq)

-- TODO maybe separate function type and value
-- Data Func = FVar Bound -- need separte enfironment to garantee func
--           | Instruc
--           | lambda  -- this become to complicated lambda can accept functions as argument
--  ...
--
-- TODO description properties
data ANorm = Appl Value [Value]
           | Val Value -- TODO maybe rename Val
           | Let [Def] ANorm -- Maybe make Distiction between orginal let (Letrec) and new Let
                             -- which should Let Def ANorm (no List)
           deriving (Show,Eq)

-- maybe make seppart constuctor for tail call
--  Tailcall l [names] ANorm
-- or
-- loop l [(names,Value)] ANorm

data Def = Def (Maybe Name) ANorm deriving (Show,Eq)

-- FIXME  take buildin in function with not enough armuegest into acount (η expand them)
aNormalize :: BruijnTerm () () -> ANorm
aNormalize = aNormalize' . reproces

aNormalize':: Unprocessed -> ANorm
aNormalize' t = case peek t of
  (ValF _ v) -> go $ reproces $ fullApplied $ Lam.Val () v-- TODO documentation, maybe split in fullApplied aNormalize
  ApplF {} -> go $ unsafeProces  t fullApplied
  _ -> go t
  where
    go :: Unprocessed -> ANorm
    go un = case peek un of
      (ValF _ (Lit.Prim v)) -> Val $ Constant v
      (ValF _ (Lit.Func f)) -> Val $ Instruc f
      LambdaF {} ->
        let (vars,body) = Unprocessed.accumulateVars un
        in foldr (\ _name _t -> Val $ Lambda _name _t) (aNormalize' body) vars
      (VarF _ b) -> Val $ Var b
      (LetF _ defs t1) -> Let (map aDefToDef defs) $ aNormalize' t1
        where
          aDefToDef (Lam.Def _ n definition) = Def (Just n) $ aNormalize' definition
      ApplF {} ->
        if null defs
        then Appl function args
        else Let defs $ Appl function args
        where
          (defs ,function:args, _) = foldr normalize ([],[],0) incLamArgs
          lamArgs = accumulateArgs un
          incLamArgs = map (go.incFree nNew) lamArgs
          nNew = length $ filter (not . atom) lamArgs

          normalize ::  ANorm -> ([Def], [Value], Int) ->([Def], [Value], Int )
          normalize et (accumDefs, accumVals, depth)= case et of -- TODO better name t
            -- Left (Var (Bound b)) -> (defs,Var (Bound $ b+nNew) : vals,n) --FIXME
            Val v -> (accumDefs,v:accumVals,depth)
            at -> (Def Nothing at: accumDefs, Var (Bound depth):accumVals,depth+1 )

etaExpansion :: Int -> [Value]-> ANorm
etaExpansion n (f:args) = applyN n (Val . Lambda (Name "#unamed")) $ Appl  f $ args ++ map (Var . Bound ) [0..n-1] -- TODO use makeTerm

applyN :: Int -> (a -> a) -> a -> a
applyN n f a
  | n <= 0 = a
  |otherwise = applyN (n - 1) f (f a)

--TODO beter name
atom :: Unprocessed -> Bool
atom t = case peek t of
  VarF {} -> True
  LambdaF {} -> True
  ValF {} -> True
  _ -> False --TODO

aValueToLambda :: Value -> BruijnTerm () ()
aValueToLambda (Lambda name t) = Lam.Lambda () name $ aToLambda t
aValueToLambda (Var b) = Lam.Var () b
aValueToLambda (Instruc f) = Lam.Val () (Lit.Func f)
aValueToLambda (Constant v) = Lam.Val () (Lit.Prim v)

aToLambda :: ANorm -> BruijnTerm () ()
aToLambda (Val v) = aValueToLambda v
aToLambda (Appl f args) = foldl Lam.Appl  (aValueToLambda f) (map aValueToLambda args)
aToLambda (Let defs t) = Lam.Let () (map aDefToDef defs) $ aToLambda t
  where
    aDefToDef (Def (Just n) at) = Lam.Def () n $ aToLambda at
    aDefToDef (Def Nothing at) = Lam.Def () (Name "#unamed") $ aToLambda at -- TODO Maybe maybe make special name

--TODO better name
accumulateVars :: ANorm -> ([Name], ANorm)
accumulateVars (Val (Lambda name t)) = first (name :) $ accumulateVars t
accumulateVars t = ([], t)

pShow :: ANorm -> String
pShow = Lam.pShow . aToLambda
