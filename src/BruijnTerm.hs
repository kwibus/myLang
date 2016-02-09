module BruijnTerm
  ( BruijnTerm
  , UndefinedVar (UndefinedVar)
  , bruijn2Lam
  , lam2Bruijn
  ) where
import Control.Monad.Except
import qualified Data.Map as M
import Data.Char
import Data.List (foldl')
import Name
import BruijnEnvironment
import Lambda

-- TODO update comment let bruijn index
--
-- | 'Lambda' term where The Bruijn Index are used.
--
-- The name of variables is here only stored in the lambda's.
--
-- (<http://en.wikipedia.org/wiki/De_Bruijn_index>)
--
-- * \\a.a == \\0
-- * \\a\\b.a == \\\\1

type BruijnTerm i = LamTerm i Bound

-- | This is error type for when a variable is used before is defined (free variable)
data UndefinedVar i n = UndefinedVar i n -- ^ i is extra information (location of variable) and n is the name
    deriving (Show, Eq)


-- | Converts 'BruijnTerm' to a 'LambTerm'
--
-- This function fails with 'Left' 'UndefinedVar' if there is a free variable.
--
-- It ad's numbers to names that otherwise would shadowed previous defined variables, so:
--
-- "\\\\1" becomes: "\\a.\\a1.a"  not: "\a.\a.a"
-- TODO used names list is inefficient maybe Map Name int
bruijn2Lam :: BruijnTerm i -> Either (UndefinedVar i Bound) (LamTerm i Name)
bruijn2Lam t = go t []
  where newName name env = head $ dropWhile (`elem` env)
            (map (\ i -> Name (name ++ i)) ("" : map show [(0 :: Int) ..] ))
            -- first name in [name,name1,name2,..] that is not allready used
        go :: BruijnTerm i -> [Name] -> Either (UndefinedVar i Bound) (LamTerm i Name)
        go (Var info n) env = case getAt env (toInt n ) of
            Nothing -> throwError $ UndefinedVar info n
            Just name -> return $ Var info name
        go (Val i v) _ = return $ Val i v
        go (Appl i e1 e2 ) env = Appl i <$> go e1 env <*> go e2 env
        go (Lambda info (Name n) e1) env =
            let name = newName n env
            in Lambda info name <$> go e1 (name : env)
        go (Let i defs t1) env = Let i <$> newDefs <*> go t1 newEnv
          where
            defNewNames = map (\ (Def i0 (Name n) t0) -> Def i0 (newName n env) t0) defs
            newDefs = mapM (\ (Def i0 n t0) -> Def i0 n <$> go t0 newEnv ) defNewNames
            newEnv :: [Name]
            newEnv = foldl' (\ env' (Def _ n _) -> n : env' ) env defNewNames

-- | Converts 'Lambda' with named variabls to 'Lambda' with Bruijn Index's
--
-- This function fails with 'Left' 'UndefinedVar' if there is a free variable.
--
-- It remove the number at the and of names ("name1"->"name") so: @lam2Bruijn bruijn2Lam == id @
--
-- the remove of index only works if there are no digits in userdevined names
-- TODO fix this maybe by rename name1 in name#1 and disallow #

lam2Bruijn :: LamTerm i Name -> Either (UndefinedVar i Name ) (BruijnTerm i)
lam2Bruijn t = go t 0 M.empty
  where removeIndex n = Name $ takeWhile (not . isDigit) n
        go :: LamTerm i Name -> Int -> M.Map Name Int -> Either (UndefinedVar i Name ) (BruijnTerm i)
        go (Var i n) depth env = case M.lookup n env of
        -- (depth - deptDefined ) is how many lamba's  back variable is defiend. -1 so first index is 0
            Just deptDefined -> return $ Var i $ Bound (depth - deptDefined - 1)
            Nothing -> throwError $ UndefinedVar i n
        go (Val i v) _ _ = return $ Val i v
        go (Lambda i (Name n) t1) depth env =
                 Lambda i (removeIndex n) <$> go t1 (depth + 1) (M.insert (Name n) depth env)
        go (Appl i t1 t2) depth env = Appl i <$> go t1 depth env <*> go t2 depth env
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


--TODO remov
-- -- env is not applied to it self
-- applyterm :: BruijnEnv (BruijnTerm i) -> BruijnTerm i -> BruijnTerm i
-- applyterm env (Appl i t1 t2) = Appl i (applyterm env t1) (applyterm env t2)
-- applyterm env (Lambda i n t) = Lambda i n (applyterm env t)
-- applyterm env (Var _ n) = bLookup n env
-- applyterm _ t = t
