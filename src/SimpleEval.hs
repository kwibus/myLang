module SimpleEval where

import BruijnTerm
import BruijnEnvironment
import Value

type Env = BruijnEnv ValR

-- TODO rename R is stupid
-- TODO Result make explicit value
-- TODO add Free
data ValR = Closure Env (BruijnTerm())
          | Result (BruijnTerm ())
          deriving Show

fullEval' :: BruijnTerm () -> BruijnTerm ()
fullEval' = toExpresion . fullEval bEmtyEnv

toExpresion :: ValR -> BruijnTerm ()
toExpresion (Closure _ _) = error "result with closure are not yet supported"
toExpresion (Result v) = v

fullEval :: Env -> BruijnTerm () -> ValR
fullEval env ast = case ast of
    Lambda {} -> Closure env ast
    (Var _ b) -> case bMaybeLookup b env of
        Just v -> v
        Nothing -> Closure env $ Var () b
    (Val _ v) -> Result $ Val () v
    (Let _ oldDefs t) ->
      let newEnv = fullEvalDefs env oldDefs
      in fullEval newEnv t
    (Appl t1 t2) -> reduce env t1 (fullEval env t2)

reduce :: Env -> BruijnTerm () -> ValR -> ValR
reduce env (Lambda _ _ t) v = fullEval (bInsert v env) t
reduce _ (Val () v1) t2 = case t2 of
    Result (Val () v2) -> Result $ Val () $ applyValue v1 v2
    Result _ -> error $ show v1 ++ "is applied with a non value: " ++ show t2
    Closure {} -> error "not supported yet"
reduce env t1 v2 = case fullEval env t1 of
    Result v1 -> reduce env v1 v2
    (Closure newEnv v1) -> reduce newEnv v1 v2

fullEvalDefs :: Env -> [Def () (BruijnTerm ())]  -> Env
fullEvalDefs env defs = snd $ foldl
    (\(b,envN) (Def _ _ t)->
        let vn = fullEval envN t
        in (b-1,bReplace (Bound b) vn envN))
    (nDefs-1,dumyEnv)
    defs
  where
    nDefs = length defs
    dumyEnv = bInserts (map (Result . implementation)  defs) env
