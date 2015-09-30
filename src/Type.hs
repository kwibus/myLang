module Type where -- TODO hide PolyType constructor

import Data.Maybe (isJust)
import Data.List
import qualified Data.IntMap as IM

import Name
import Environment

type Type = PolyType
data TypeInstance = TDouble deriving (Eq, Show)

data MonoType = TVal TypeInstance
  | TVar Int
  | TAppl MonoType MonoType deriving (Eq, Show)

data PolyType = PolyType { dic :: Dictionary, monotype :: MonoType}

type Dictionary = IM.IntMap String

toPoly :: MonoType -> PolyType
toPoly = PolyType IM.empty

typeVars = nub . getTvars
  where getTvars (TVar i) = [i]
        getTvars (TAppl i j) = getTvars i ++ getTvars j
        getTvars (TVal {}) = []

size :: MonoType -> Int
size (TVal {}) = 1
size (TVar {}) = 1
size (TAppl t1 t2) = size t1 + size t2

tDrop :: PolyType -> PolyType
tDrop (PolyType e (TAppl _ t )) = PolyType e t
tDrop _ = error "apply non function"

-- unpredictabel if a type variable  has multiply names
mkDictonarie :: [PolyType] -> Dictionary
mkDictonarie = IM.unions . map dic


pShow :: PolyType -> Dictionary -> String
pShow (PolyType _ t ) dic0 = fst $ go t letters dic0
  where
    go :: MonoType -> [String] -> Dictionary -> (String, ([String], Dictionary))
    go (TVar i) freeNames dic = case IM.lookup i dic of
        Just name -> (name, (freeNames, dic))
        Nothing ->
            let name : newFreeNames = dropWhile (\ n -> isJust (IM.lookup n dic)) freeNames
            in (name (newFreeNames, insert name i dic ))
    go (TAppl t1 t2) freeNames dic =
      let (string1, state1) = go t1 freeNames dic
          (string2, state2) = uncurry (go t2 ) state1
      in (string1 ++ "-->" ++ string2, state2)


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
--                 Just str -> return str
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
