module BruijnTerm
  ( module Lambda
  , Bound (..)
  , BruijnTerm
  , UndefinedVar (UndefinedVar)
  , bruijn2Lam
  , lam2Bruijn
  , defsBounds
  , pShow
  , incFree
  , incFreeOfset
  , fullApplied --TODO rename
  ) where

import Control.Monad.Except
import qualified Data.Map as M
import Data.Char
import Data.List (foldl')

import Info
import Name
import Value
import BruijnEnvironment
import Lambda
import qualified PrettyPrint as Lambda

-- | 'Lambda' term where The Bruijn Index are used.
--
-- The name of variables is here only stored in the lambda's.
--
-- (<http://en.wikipedia.org/wiki/De_Bruijn_index>)
--
-- * \\a.a == \\0
-- * \\a\\b.a == \\\\1

type BruijnTerm i j = LamTerm i j Bound

-- | This is error type for when a variable is used before is defined (free variable)
data UndefinedVar i n = UndefinedVar i n -- ^ i is extra information (location of variable) and n is the name
    deriving (Show, Eq)

pShow :: BruijnTerm i j-> String
pShow a = either (const (show $ removeInfo a)) Lambda.pShow $ bruijn2Lam a

-- | Converts 'BruijnTerm' to a 'LambTerm'
--
-- This function fails with 'Left' 'UndefinedVar' if there is a free variable.
--
-- It ad's numbers to names that otherwise would shadowed previous defined variables, so:
--
-- "\\\\1" becomes: "\\a.\\a1.a"  not: "\a.\a.a"
-- TODO used names list is inefficient maybe Map Name int??
bruijn2Lam :: BruijnTerm i j -> Either (UndefinedVar j Bound) (LamTerm i j Name)
bruijn2Lam t = go t []
    where
        -- first name in [name,name1,name2,..] that is not allready used
        mkNewName name env = head $ dropWhile (`elem` env)
                (map (\ i -> fromString (toString name ++ i)) ("" : map show [(0 :: Int) ..] ))
        go :: BruijnTerm i j -> [Name] -> Either (UndefinedVar j Bound) (LamTerm i j Name)
        go (Var info n) env = case getAt env (toInt n ) of
            Nothing -> throwError $ UndefinedVar info n
            Just name -> return $ Var info name
        go (Val i v) _ = return $ Val i v
        go (Appl e1 e2 ) env = Appl <$> go e1 env <*> go e2 env
        go (Lambda info name e1) env =
            let newName = mkNewName name env
            in Lambda info newName <$> go e1 (newName : env)
        go (Let i defs t1) env = Let i <$> newDefs <*> go t1 newEnv
          where
            newDefs = reverse <$> mapM
                (\ (newN, Def i0 _ t0) -> Def i0 newN <$> go t0 newEnv)
                (zip newNames $ reverse defs)
            newEnv = newNames ++ env
            newNames :: [Name]
            newNames = foldl' (\ env' (Def _ n _) -> mkNewName n (env ++ env') : env') [] defs

-- | Converts 'Lambda' with named variabls to 'Lambda' with Bruijn Index's
--
-- This function fails with 'Left' 'UndefinedVar' if there is a free variable.
--
-- It remove the number at the and of names ("name1"->"name") so: @lam2Bruijn bruijn2Lam == id @
--
-- the remove of index only works if there are no digits in userdevined names
-- TODO fix this maybe by rename name1 in name#1 and disallow #

lam2Bruijn :: LamTerm i j Name -> Either (UndefinedVar j Name ) (BruijnTerm i j)
lam2Bruijn t = go t 0 M.empty
  where removeIndex n = fromString $ takeWhile (not . isDigit) n
        go :: LamTerm i j Name -> Int -> M.Map Name Int -> Either (UndefinedVar j Name ) (BruijnTerm i j)
        go (Var i n) depth env = case M.lookup n env of
        -- (depth - deptDefined ) is how many lamba's  back variable is defiend. -1 so first index is 0
            Just deptDefined -> return $ Var i $ Bound (depth - deptDefined - 1)
            Nothing -> throwError $ UndefinedVar i n
        go (Val i v) _ _ = return $ Val i v
        go (Lambda i name t1) depth env =
                 Lambda i (removeIndex (toString name)) <$> go t1 (depth + 1) (M.insert name depth env)
        go (Appl t1 t2) depth env = Appl <$> go t1 depth env <*> go t2 depth env
        go (Let i defs t1) depth env = Let i <$> newDefs <*> go t1 newDepth newEnv
          where
            newDepth = depth + length defs
            newDefs = mapM (\ (Def i0 (Name n) t0) -> Def i0 (removeIndex n) <$> go t0 newDepth newEnv) defs
            newEnv :: M.Map Name Int
            newEnv = foldl' (\ env' (Def _ n _, index) -> M.insert n index env' ) env $ zip defs [depth ..]

-- TODO move to a utility module, or replace with libary function
getAt :: [a] -> Int -> Maybe a
getAt [] _ = Nothing
getAt (x : _) 0 = Just x
getAt (_ : xs) n = getAt xs (n - 1)

defsBounds :: [a] -> [Bound]
defsBounds defs = Bound <$> fromToZero (length defs - 1)

incFree :: Int -> BruijnTerm i j -> BruijnTerm i j
incFree = incFreeOfset 0

incFreeOfset :: Int -> Int -> BruijnTerm i j -> BruijnTerm i j
incFreeOfset _ 0 term = term

incFreeOfset ofset increase term = go ofset term
  where
    go :: Int -> BruijnTerm i j -> BruijnTerm i j
    go depth (Lambda i n t) = Lambda i n $ go (depth + 1) t
    go depth (Appl t1 t2) = Appl (go depth t1) (go depth t2)
    go depth (Var i (Bound n))
        | n >= depth = Var i $ Bound $ n + increase
        | otherwise = Var i (Bound n)
    go depth (Let i defs t) = Let i (map (fmap $ go newDepth) defs) $ go newDepth t
      where
        newDepth = depth + length defs
    go _ (Val i v) = Val i v

 -- TODO use makeTerm
 -- TODO incfree slow
fullApplied :: BruijnTerm () () -> BruijnTerm () ()
fullApplied t@(Val _ (Func f)) = case arrity f of
  0 -> Val () $ Func f
  2 -> Lambda () DummyBegin $ Lambda () DummyEnd $
        foldl Appl (incFree 2 t) $ map (Var (). Bound ) [0,1]
  _ -> error "not supported yet" --etaExpansion n t
fullApplied t@Appl{} = case accumulateArgs t of
  (Val _ (Func f): rest)
    | arrity f - length rest == 0 -> t
    | arrity f - length rest == 1 -> Lambda () DummyEnd $
        Appl (incFree 1 t) (Var () $ Bound 0)
    -- %| otherwise -> etaExpansion (arrity f - length rest) t
  _ -> t -- TODO need type information to expand no buildin fucntions
fullApplied t = t
