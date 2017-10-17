module Type where

import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Set as Set

import Data.List
import Name
import FreeEnvironment

type Dictionary = FreeEnv String
type Type = TypeA ()

--TODO maybe rename pritive types
--TODO maybe make llvm type
data TypeInstance = TDouble
                  | TBool
                  deriving (Eq, Show)

-- TODO replace i by Free ?
-- TODO merge tvar poly with (TVar kind i)
data TypeA i = TVal TypeInstance
            | TVar Free i
            | TPoly Free i
            | TAppl (TypeA i) (TypeA i) deriving (Eq, Show)

normalise :: Type -> Type
normalise = snd .normaliseWith IM.empty

-- TODO not a save interface unMapped types get number assignd base on size map, which already can be use
--      you could use list instead, or only use normalise
-- TODO rename match function closer
-- TODO it recalculates size every time, could be more efficent. but wont matter in practice
normaliseWith :: IM.IntMap Type -> Type -> (IM.IntMap Type, Type)
normaliseWith env t@TVal {} = (env,t)
normaliseWith env (TPoly (Free f) _) = case IM.lookup f env of
      Just t@TPoly {} -> (env,t)
      Just _ -> error "key clash"
      Nothing -> let t =  TPoly (Free $ IM.size env) () in (IM.insert f t env,t)
normaliseWith env (TVar (Free f) _) = case IM.lookup f env of
      Just t@TVar {} -> (env,t)
      Just _ -> error "keyclash"
      Nothing -> let t =  TVar (Free $ IM.size env) () in (IM.insert f t env,t)
normaliseWith env (TAppl t1 t2) =
  let (env', t1') = normaliseWith env t1
      (env'',t2') = normaliseWith env' t2
  in (env'', TAppl t1' t2')

mkDictonarie :: [Type] -> Dictionary
mkDictonarie = mkDictonarieWithReserved IM.empty

mkDictonarieWithReserved :: Dictionary -> [Type] -> Dictionary
mkDictonarieWithReserved fixedNames ts = fst $ foldl go (fixedNames, letters ) $ concatMap typeVars ts
  where
    go :: ( Dictionary, [String] ) -> Free -> ( Dictionary, [String])
    go (dic, freeNames) (Free i) = case IM.lookup i dic of
        Just _ -> (dic, freeNames)
        Nothing ->
            let usedNames = map snd $ IM.toList dic
                name : newFreeNames = dropWhile (\ n -> elem n usedNames) freeNames
            in (IM.insert i name dic, newFreeNames)

pShow :: Type -> String
pShow t = pShowWithDic t (mkDictonarie [t])

pShowWithDic :: TypeA () -> Dictionary -> String
pShowWithDic = go
  where
    go :: TypeA () -> Dictionary -> String
    go (TPoly i f) dic = '*':go (TVar i f ) dic
    go (TVar (Free i) _) dic = fromMaybe
                  (error "incomplete dictonary; missing name for: " ++ show i)
                  (IM.lookup i dic)
    go (TAppl t1 t2) dic =
      let string1 = case t1 of
            TAppl {} -> "(" ++ go t1 dic ++ ")"
            _ -> go t1 dic
          string2 = go t2 dic
      in string1 ++ " -> " ++ string2
    go (TVal v) _ = showTypeInstance v

typeVars :: Eq i => TypeA i -> [Free]
typeVars = nub . getTvars
  where getTvars (TVar i _) = [i]
        getTvars (TAppl i j) = getTvars i ++ getTvars j
        getTvars (TPoly i _) = [i]
        getTvars TVal {} = []

typeSize :: TypeA i -> Int
typeSize (TAppl t1 t2) = typeSize t1 + typeSize t2
typeSize _ = 1

-- TODO rename it does not map var anymore but anotation
mapVar :: (i -> j) -> TypeA i -> TypeA j
mapVar f (TAppl t1 t2) = TAppl (mapVar f t1) (mapVar f t2)
mapVar f (TPoly i j) = TPoly i $ f j
mapVar f (TVar i j) = TVar i $ f j
mapVar _ (TVal a) = TVal a

mapFree :: (Free -> Free) -> TypeA i -> TypeA i --T
mapFree f (TAppl t1 t2) = TAppl (mapFree f t1) (mapFree f t2)
mapFree f (TPoly i j) = TPoly (f i) j
mapFree f (TVar i j) = TVar (f i) j
mapFree _ (TVal a) = TVal a

showTypeInstance :: TypeInstance -> String
showTypeInstance TDouble = "Double"
showTypeInstance TBool = "Bool"

dropTypeArg :: Show i => TypeA i -> TypeA i
dropTypeArg (TAppl _ t ) = t
dropTypeArg t = error $ "cant drop targ with"  ++ show t

typeFreeVars ::Type -> Set.Set Free
typeFreeVars (TVar v _ ) = Set.singleton v
typeFreeVars (TAppl t1 t2) = typeFreeVars t1 `Set.union` typeFreeVars t2
typeFreeVars _ = Set.empty

accumulateTypes :: TypeA a -> [TypeA a]
accumulateTypes (TAppl t1 t2) = t1 : accumulateTypes t2
accumulateTypes t = [t]
