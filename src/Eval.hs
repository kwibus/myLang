{-# LANGUAGE TupleSections, LambdaCase, DeriveFunctor #-}
module Eval
where


import Debug
import Debug.Trace

import Control.Monad.Writer.Lazy
import Control.Monad.State.Strict
import Data.Maybe
import Data.Bifunctor

import Lambda
import BruijnTerm
import Value
import BruijnEnvironment
import Type

-- TODO add Nothing for outof scope?
data Ref a = Subst a | Keep  a deriving (Show, Eq,Functor)
type Scope i = BruijnEnv (Ref (BruijnTerm i))

unwrap :: Ref a -> a
unwrap (Subst a) = a
unwrap (Keep a ) = a

extract :: Bound  ->  Scope i -> BruijnTerm i
extract b env = fromMaybe (error $ show b ++ " not in env") $ tryExtract b env

tryExtract :: Bound -> Scope i -> Maybe (BruijnTerm i)
tryExtract b env = case  bMaybeLookup b env of
    (Just (Subst t)) -> Just $ incFree (depth +1) t
    (Just (Keep t)) -> Just $ incFree depth t
    Nothing -> Nothing
  where depth = getDepth b env

-- ref :: BruijnTerm i -> Ref (BruijnTerm i)
-- ref term | isValue term = Subst term
--          | otherwise = Keep term

type Evaluator a = Writer [a] a

emap :: (a -> b) -> Evaluator a -> Evaluator b
emap f e = retell (map f) ( f <$> e)

produce :: a -> Evaluator a
produce a = tell [a] >> return a

retell  :: (w1 -> w2) -> Writer w1 a-> Writer w2 a
retell = mapWriter . second
--
-- TODO write Test  for correct order
-- |eval term in accordance with call by value.
-- If a term can't be further be evaluated it will return 'Nothing'
eval :: BruijnTerm () -> Maybe (BruijnTerm ())
eval = listToMaybe . evalSteps

evalSteps ::BruijnTerm () -> [BruijnTerm ()]
evalSteps = fmap fst . snd  . runWriter . evalWithEnv' bEmtyEnv

--TODO fix names
--FIXME  way to complex
-- tricks used:
-- * lazy substitute: delay substitute till needed
--      why: is faster if you only need last one,and comparable if you need next step
--      how: store substitute in map
--           lowering bruijen subst
--
--           you never have to lower a variable that you gone substitute; just substitute.
--           what you substitute does not have to be lowered, because you perform substitute before you insert
--           and it cant contain newer variables because those were not defined when you defined substitute
--
--           lowering bruijen non subst variables
--           see next first
--           before you substitute start replace keeps with subst with lowering
--
-- * eval defenitions only when you encounter one
--      why: defenitions can depend on each other: and this way you dont have to sort defenitions first
--      how: store defenitions in env, end eval lookup when needed
-- * delayed substitute and delay defenitions  evaluations are stored in the same map
--      why: only values(fullEvalutated) / lambdavalues are substitute so the sets disjunct
--
--

evalWithEnv :: Scope () -> BruijnTerm () -> Evaluator (BruijnTerm (), Scope ())
evalWithEnv env term = evalWithEnv' env term -- >>= deref

evalWithEnv' :: Scope () -> BruijnTerm () -> Evaluator (BruijnTerm (), Scope ())
evalWithEnv' env  term | traceShow (printBrujin term,env)  False = undefined
evalWithEnv' env (Appl func args) =
  do
    (right, newerEnv) <- censor (map (first (Appl (substituteLowerEnv env func) ))) $ evalWithEnv env args
    (left, newerEnv2) <- censor (map (first  $ flip Appl right )) $ evalWithEnv (matchLevels env newerEnv) func
    case left of
          (Lambda _ _ t1) -> do
                             let newestEnv = bInsert (Subst $ right) newerEnv2
                             tell [(substituteLowerEnv newestEnv t1,newerEnv2)]
                             evalWithEnv newestEnv t1
          (Val i1 v1) -> produce (Val i1 $ applyValue v1 $ value $ right, newerEnv2)
          -- (Var _ b)  -> let (outscope,inscope) = bSplitAt b env -- TODO extra step
          --               in second (bAppend outscope) <$> final (extract b env) inscope
          _ -> error "afsd"

evalWithEnv' env (Let info defs term) =
     do
        result <- censor (map $uncurry prependLet) $ evalWithEnv newEnv term
        -- TODO maybe drop some variables env
        final result
    where
    level :: Int
    level = levels dumyEnv
    prependLet _term _env = (Let info (updateDefs _env) _term, _env)
    updateDefs _env = zipWith
      (\ (Def _info n _) index -> Def _info n $ unwrap $ bLookup index (matchLevels dumyEnv _env))
      defs
      (defsBounds defs)
    newEnv =  bInserts (reverse $ fmap (Keep . substituteLowerEnv dumyEnv . implementation) defs ) env
    dumyEnv :: Scope  ()
    dumyEnv = bInserts ( Keep . Var () <$> reverse (defsBounds  defs )) env --TODO find clean solution
    final result = case result of
        (v@Val {},_env) -> produce (v, _env)
        (Lambda _info n t,_env) ->
                let newt = swap (length defs) $ substituteLowerEnv env1 t
                    newenv = mapLevel (level-1)  (fmap (incFree' (length defs) 1 )) _env
                    env1 =    bInsert (Keep$ Var () $ Bound $ 0) newenv
                    -- t
                        -- substituteLowerEnv (bInsert (Subst $ Var () $ Bound $ length defs) _env) t
                    --     substitute decreas (bInsert  (Subst  $ Var () (Bound $ length defs)) env1) t
                    -- (decreas ,env1) = lowerEnv _env
                in produce $ first (Lambda _info n ) (prependLet newt newenv)
        -- (Var _ b, _env) -> final ((extract b _env), _env)
        -- (t@Var {},env) -> return (substituteEnv env t,env )
        -- t(Var _ b,_env)-> case tryExtract b _env of
        --        Nothing -> error "not in scope 2"
        --        Just Var {}->error "not in scope"
        --        -- Just v -> final (v,_env)
        _ -> return $ uncurry prependLet result

evalWithEnv' env t@(Var _ b) =
    case bMaybeLookup  b env of
        Just (Subst _) -> return (extract b env,env)
        Just (Keep term) -> do
            (newTerm,newInScope) <- censor(map ((substituteLowerEnv env t,) . extendAndreplace)) $ evalWithEnv' inScope term
            let  incTerm = incFree depth newTerm
            tell [(substituteLowerEnv  env incTerm, extendAndreplace (newTerm,newInScope))]
            return (incTerm,extendAndreplace (newTerm,newInScope))
        Nothing -> return (substituteLowerEnv env t,env)
    where (outScoop, inScope) = bSplitAt b env
          extendAndreplace :: (BruijnTerm () ,Scope ()) -> Scope ()
          extendAndreplace = uncurry (bReplace b) . bimap Keep reExtend
          reExtend =bAppend outScoop . matchLevels inScope
          depth = getDepth b env
    -- do
    -- finalenv <- retell ((substituteEnv env t,)) $ updateEnv b env -- substituteEnv can smarter
    -- return (t,finalenv )

evalWithEnv' env t@Lambda {} = return (substituteEnv env t,env)
evalWithEnv' env t = return (t,env)

-- deref :: (BruijnTerm (), Scope ()) -> Evaluator ( BruijnTerm (),Scope () )
-- deref (term,env) = return . (,env) $ case term of
--             (Var _ b) -> fromMaybe term  $ tryExtract b env
--            _ -> term

-- variable to update
updateEnv ::Bound -> Scope () -> Evaluator (Scope ())
-- updateEnv (Bound b)  env | traceShow  (b, env) False = undefined
updateEnv b env = case bMaybeLookup  b env of
    Just (Subst _) -> return env

    -- Just (Keep (Var _ b2)) -> do
    --     newestInscope <- censor (map reExtend) $ updateEnv b2 inScope
    --     produce  $ bReplace b (Keep $ extract b2  newestInscope) $ reExtend newestInscope
--
    Just (Keep term) ->  do
        (newTerm,newInScope) <-retell (map extendAndreplace) $ evalWithEnv' inScope term
        case newTerm of
            (Var _ b2) -> do
                newestInscope<- censor (map reExtend) $ updateEnv b2 newInScope
                produce  $ bReplace b (Keep $ extract b2  newestInscope) $ reExtend newestInscope -- TODO tryextrace
            _ -> return $ extendAndreplace (newTerm,newInScope)
    --
    -- Just (Keep term) -> case term of
    --         (Var _ b2)  -> do
    --             newInScope <- censor (map $ bAppend outScoop . matchLevels inScope ) $ updateEnv b2 inScope
    --             produce $ bReplace b (Keep$ extract b2  newInScope) (bAppend outScoop  $ matchLevels inScope newInScope )
    --         _ -> emap (uncurry (bReplace b) . bimap Keep (bAppend outScoop . matchLevels inScope)) $ evalWithEnv inScope term

    Nothing -> return env
    where (outScoop, inScope) = bSplitAt b env
          extendAndreplace :: (BruijnTerm () ,Scope ()) -> Scope ()
          extendAndreplace = uncurry (bReplace b) . bimap Keep reExtend
          reExtend =bAppend outScoop . matchLevels inScope

-- TODO can skip if substituteEnv if subs = 0
substituteLowerEnv :: Scope () -> BruijnTerm () -> BruijnTerm ()
substituteLowerEnv env = uncurry substitute (lowerEnv  env)


lowerEnv :: Scope () -> (Int,Scope ())
lowerEnv env=  (subs,newEnv)
    where ((_,subs),newEnv) = transforml
                    (\(i,sub) t -> case t of
                            Subst s -> ((i + 1,sub + 1),Subst s)
                            Keep {} -> ((i+ 1,sub ),Subst $ Var () $ Bound $ i-sub )
                    ) (0::Int,0::Int) env

substituteEnv :: Scope () -> BruijnTerm () -> BruijnTerm ()
substituteEnv = substitute 0

substitute :: Int -> Scope () -> BruijnTerm () -> BruijnTerm ()
substitute  inc env term
    | bNull env = term
    | otherwise = go 0 env term
    where
        go depth e (Lambda i n t) = Lambda i n $ go (depth + 1) e t
        go _     _ t@Val {} = t
        go depth e (Appl left right) = Appl (go depth e left) (go depth e right)

        go depth e t@(Var _ (Bound n))
            | n >= depth = case bMaybeLookup (Bound (n-depth)) e of
                               Just (Subst v) -> incFree depth v
                               _ -> Var () $ Bound $!n - inc
            | otherwise = t
        go depth e (Let i defs t) = Let i (fmap goDefs defs) $ go' t
            where go' = go (depth + length defs) e
                  goDefs ( Def i' n' t') = Def i' n' $ go' t'

--TODO can this be done in env
swap :: Int -> BruijnTerm () -> BruijnTerm ()
swap n = go 0
  where go depth (Lambda _ name t) = Lambda () name $ go (depth + 1) t
        go depth (Appl t1 t2) = Appl (go depth t1) (go depth t2)
        go  _    t@Val {} = t
        go depth (Var _ (Bound n2))
            | n2 == depth = Var () (Bound $! depth + n)
            | depth < n2 && depth + n >= n2 = Var () (Bound (n2 -1))
            | otherwise  = Var ()  (Bound n2)
        go depth (Let _ defs term) = Let () (fmap goDefs defs) $ go' term
            where go' = go (depth  + length defs)
                  goDefs (Def i name t) = Def i name $ go' t

-- substitute :: BruijnTerm () ->  BruijnTerm () -> BruijnTerm ()
-- substitute t1 = substituteEnv (bInsert t1 bEmtyEnv)

value :: BruijnTerm () -> Value
value (Val _ v ) = v
value t = error $ show t ++ " is not a value"

-- | applys a build in function to one argument
--  It crashes if first argument is not a function (It only export to include int test)
applyValue :: Value -- ^ build in function
  -> Value -- ^ argument
  -> Value -- ^ result
applyValue BuildIn {arrity = 1, evaluator = e, stack = s } v = evalState e (v : s )
applyValue v1@BuildIn {arrity = n, stack = s, myType = t } v2 =
    v1 {arrity = n - 1 , stack = v2 : s, myType = dropTypeArg t}
applyValue _ _ = error "apply value"

isValue :: BruijnTerm i -> Bool
isValue Var {} = False --TODO check
isValue Val {} = True
isValue Lambda {} = True
isValue Appl {} = False
isValue Let {} = False

-- TODO  beter name/description
-- increase every variale name in term that not bound in that therm with increase
incFree :: Int -> BruijnTerm i -> BruijnTerm i
incFree 0 term = term
incFree increase  term = go 0 term
  where go depth (Lambda i n t) = Lambda i n $ go (depth+1) t
        go depth (Appl t1 t2) = Appl (go depth t1)(go depth t2)
        go depth (Var i (Bound n)) | n >= depth  = Var i $ Bound $ n+increase
                                   | otherwise = Var i (Bound n)
        go depth (Let i defs t) = Let i (fmap incDefs defs) $ go newDepth t
          where
            newDepth = depth +length defs
            incDefs (Def is ns ts) = Def is ns $ go newDepth ts
        go _ (Val i v) = Val i v

--FIXME duplcated incfree
incFree' ::Int -> Int -> BruijnTerm i -> BruijnTerm i
incFree' ofset increase term = go ofset term
  where go depth (Lambda i n t) = Lambda i n $ go (depth+1) t
        go depth (Appl t1 t2) = Appl (go depth t1)(go depth t2)
        go depth (Var i (Bound n)) | n >= depth  = Var i $ Bound $ n+increase
                                   | otherwise = Var i (Bound n)
        go depth (Let i defs t) = Let i (fmap incDefs defs) $ go newDepth t
          where
            newDepth = depth +length defs
            incDefs (Def is ns ts) = Def is ns $ go newDepth ts
        go _ (Val i v) = Val i v

fullEval :: BruijnTerm () -> BruijnTerm ()
fullEval t = fst $ fst $ runWriter ( evalWithEnv' bEmtyEnv t )
