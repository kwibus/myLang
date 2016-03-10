module Type where

import qualified Data.IntMap as IM
import Data.Maybe

import Data.List
import Name
import FreeEnvironment

type Dictionary = FreeEnv String
type Type = TypeA Free


data TypeInstance = TDouble
                  | TBool
                  deriving (Eq, Show)

-- TODO replace i by Free ?
-- TODO merge tvar poly with (TVar kind i)
data TypeA i = TVal TypeInstance
            | TVar i
            | TPoly i
            | TAppl (TypeA i) (TypeA i) deriving (Eq, Show)

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

pShowWithDic :: Type -> Dictionary -> String
pShowWithDic = go
  where
    go (TPoly f) dic = '*':go (TVar f ) dic
    go (TVar (Free i)) dic = fromMaybe
                  (error "incomplete dictonary; missing name for: " ++ show i)
                  (IM.lookup i dic)
    go (TAppl t1 t2) dic =
      let string1 = case t1 of
            TAppl {} -> "(" ++ go t1 dic ++ ")"
            _ -> go t1 dic
          string2 = go t2 dic
      in string1 ++ " -> " ++ string2
    go (TVal v) _ = showTypeInstance v


typeVars :: Eq i => TypeA i -> [i]
typeVars = nub . getTvars
  where getTvars (TVar i) = [i]
        getTvars (TAppl i j) = getTvars i ++ getTvars j
        getTvars (TPoly i) = [i]
        getTvars TVal {} = []

typeSize :: TypeA i -> Int
typeSize (TAppl t1 t2) = typeSize t1 + typeSize t2
typeSize _ = 1

mapVar :: (i -> j) -> TypeA i -> TypeA j
mapVar f (TAppl t1 t2) = TAppl (mapVar f t1) (mapVar f t2)
mapVar f (TPoly i) = TPoly (f i)
mapVar f (TVar i) = TVar (f i)
mapVar _ (TVal a) = TVal a

showTypeInstance :: TypeInstance -> String
showTypeInstance TDouble = "Double"
showTypeInstance TBool = "Bool"

dropTypeArg :: TypeA i -> TypeA i
dropTypeArg (TAppl _ t ) = t
dropTypeArg _ = error "apply non function"
