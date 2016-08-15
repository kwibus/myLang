module BruijnTerm
  ( BruijnTerm
  , bruijn2Lam
  , lam2Bruijn
  , defsBounds
  ) where

import Control.Monad.Except
import qualified Data.Map as M
import Data.Char
import Data.List (foldl')

import Error
import Lam1
import Name
import BruijnEnvironment
import Lambda

-- | 'Lambda' term where The Bruijn Index are used.
--
-- The name of variables is here only stored in the lambda's.
--
-- (<http://en.wikipedia.org/wiki/De_Bruijn_index>)
--
-- * \\a.a == \\0
-- * \\a\\b.a == \\\\1

type BruijnTerm i = LamTerm Pattern i Bound

-- | Converts 'BruijnTerm' to a 'LambTerm'
--
-- This function fails with 'Left' 'UndefinedVar' if there is a free variable.
--
-- It ad's numbers to names that otherwise would shadowed previous defined variables, so:
--
-- "\\\\1" becomes: "\\a.\\a1.a"  not: "\a.\a.a"
-- TODO used names list is inefficient maybe Map Name int??
bruijn2Lam :: HasName v => LamTerm v i Bound -> Either (UndefinedVar i Bound) (LamTerm v i Name)
bruijn2Lam t = go t []
    where
        -- first name in [name,name1,name2,..] that is not allready used
        mkNewName name env = head $ dropWhile (`elem` env)
                (map (\ i -> fromString (toString name ++ i)) ("" : map show [(0 :: Int) ..] ))
        go :: HasName v =>LamTerm v i Bound -> [Name] -> Either (UndefinedVar i Bound) (LamTerm v i Name)
        go (Var info n) env = case getAt env (toInt n ) of
            Nothing -> throwError $ UndefinedVar info n
            Just name -> return $ Var info name
        go (Lit i v) _ = return $ Lit i v
        go (Appl e1 e2 ) env = Appl <$> go e1 env <*> go e2 env
        go (Lambda n e1) env =
            let newName = mkNewName (getName n )env
            in Lambda (setName n newName) <$> go e1 (newName : env)
        go (Let i defs t1) env = Let i <$> newDefs <*> go t1 newEnv
          where
            newDefs = reverse <$> mapM
                (\ (newN,Def v t0) -> Def (setName v newN) <$> go t0 newEnv)
                (zip newNames $ reverse defs)
            newEnv =  newNames ++ env
            newNames :: [Name]
            newNames = foldl'  (\ env' (Def n _) ->  mkNewName (getName n) (env++env'): env')  [] defs

-- | Converts 'Lambda' with named variabls to 'Lambda' with Bruijn Index's
--
-- This function fails with 'Left' 'UndefinedVar' if there is a free variable.
--
-- It remove the number at the and of names ("name1"->"name") so: @lam2Bruijn bruijn2Lam == id @
--
-- the remove of index only works if there are no digits in userdevined names
-- TODO fix this maybe by rename name1 in name#1 and disallow #

lam2Bruijn :: HasName v => LamTerm v i Name -> Either (UndefinedVar i Name ) (LamTerm v i Bound)
lam2Bruijn t = go t 0 M.empty
  where removeIndex n = fromString $ takeWhile (not . isDigit) n
        go :: HasName v => LamTerm v i Name -> Int -> M.Map Name Int -> Either (UndefinedVar i Name ) (LamTerm v i Bound)
        go (Var i n) depth env = case M.lookup n env of
        -- (depth - deptDefined ) is how many lamba's  back variable is defiend. -1 so first index is 0
            Just deptDefined -> return $ Var i $ Bound (depth - deptDefined - 1)
            Nothing -> throwError $ UndefinedVar i n
        go (Lit i v) _ _ = return $ Lit i v
        go (Lambda v  t1) depth env =
            let name = getName v
            in Lambda (setName v (removeIndex $toString name))
                 <$> go t1 (depth + 1) (M.insert name depth env)
        go (Appl t1 t2) depth env = Appl <$> go t1 depth env <*> go t2 depth env
        go (Let i defs t1) depth env = Let i <$> newDefs <*> go t1 newDepth newEnv
          where
            newDepth = depth + length defs
            newDefs = mapM (\ (Def v t0) -> Def (setName v $ removeIndex $ toString $getName v )  <$> go t0 newDepth newEnv) defs
            newEnv :: M.Map Name Int
            newEnv = foldl' (\ env' (Def v _, index) -> M.insert (getName v) index env' ) env $ zip defs [depth ..]

-- TODO move to a utility module, or replace with libary function
getAt :: [a] -> Int -> Maybe a
getAt [] _ = Nothing
getAt (x : _) 0 = Just x
getAt (_ : xs) n = getAt xs (n - 1)


fromToZero :: Int -> [Int]
fromToZero n | n < 0 = []
             | otherwise = n : fromToZero (pred  n)

defsBounds :: [Def v i Bound] -> [Bound]
defsBounds defs = Bound <$> fromToZero (length defs - 1)
