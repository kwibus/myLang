module Type where -- TODO hide PolyType constructor

import Data.Maybe (fromMaybe)
import Data.List
import qualified Data.IntMap as IM

import Name
import Environment

data TypeInstance = TDouble deriving (Eq, Show)

data MonoType = TVal TypeInstance
  | TVar Int
  | TAppl MonoType MonoType deriving (Eq, Show)

data PolyType = PolyType { dic :: Dictionary, monotype :: MonoType} deriving Show

type Dictionary = IM.IntMap String

toPoly :: MonoType -> PolyType
toPoly = PolyType IM.empty

toMono :: PolyType -> MonoType
toMono (PolyType _ t) = t

typeVars :: MonoType -> [Int]
typeVars = nub . getTvars
  where getTvars (TVar i) = [i]
        getTvars (TAppl i j) = getTvars i ++ getTvars j
        getTvars (TVal {}) = []

size :: MonoType -> Int
size (TVal {}) = 1
size (TVar {}) = 1
size (TAppl t1 t2) = size t1 + size t2

dropTypeArg :: PolyType -> PolyType
dropTypeArg (PolyType e (TAppl _ t )) = PolyType e t
dropTypeArg _ = error "apply non function"

-- unpredictabel if a type variable  has multiply names
mkDictonarie :: [PolyType] -> Dictionary
mkDictonarie ts = fst $ foldr go (fixedNames, letters ) $ concatMap (typeVars . toMono) ts
  where
    go :: Int -> ( Dictionary, [String] ) -> ( Dictionary, [String])
    go i (dic, freeNames) = case IM.lookup i dic of
        Just name -> (dic, freeNames)
        Nothing ->
            let usedNames = map snd $ IM.toList dic
                name : newFreeNames = dropWhile (\ n -> (elem n usedNames )) freeNames
            in (IM.insert i name dic, newFreeNames)
    fixedNames = IM.unions $ map dic ts


pShow :: PolyType -> Dictionary -> String
pShow (PolyType _ t ) dic0 = go t dic0
  where
    go :: MonoType -> Dictionary -> String
    go (TVar i) dic = fromMaybe
                  (error "incomplete dictonary; missing name for: " ++ show i)
                  (IM.lookup i dic)
    go (TAppl t1 t2) dic =
      let string1 = go t1 dic
          string2 = go t2 dic
      in string1 ++ "-->" ++ string2
    go (TVal v) _ = showTypeInstance v

type Free = Int
-- pShow :: PolyType -> String
-- pShow t = evalState (showEnv t) initState
--
-- -- FIXME this is incorect for (TYPE Boud) toInt
-- showEnv :: PolyType -> State (IM.IntMap String , [String]) String
-- showEnv (TVal v) = return (showTypeInstance v)
-- showEnv (TAppl t1 t2) = do
--             s1 <- showEnv t1
--             s2 <- showEnv t2
--             let s1' = case t1 of
--                  TAppl {} -> "(" ++ s1 ++ ")"
--                  _ -> s1
--             return $ s1' ++ " -> " ++ s2
-- showEnv (TVar i ) = do
--             (m, names) <- get
--             case IM.lookup (toInt i) m of
--                 Just etr -> return str
--                 Nothing ->
--                     let newname = head names
--                         newMap = IM.insert (toInt i) newname m
--                     in put (newMap, tail names) >> return newname
--
-- initState :: (IM.IntMap String, [String] )
-- initState = (IM.empty, letters)
--
-- genNames :: PolyType -> State (IM.IntMap String , [String]) ()
-- genNames (TVal _) = return ()
-- genNames (TVar i) = do
--     (m , names) <- get
--     put (IM.insert (toInt i) (head names) m, init names)
-- genNames (TAppl t1 t2) = genNames t1 >> genNames t2

showTypeInstance :: TypeInstance -> String
showTypeInstance TDouble = "Double"
