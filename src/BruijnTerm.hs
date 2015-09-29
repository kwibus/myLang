module BruijnTerm
  ( BruijnTerm
  , UndefinedVar (UndefinedVar)
  , bruijn2Lam
  , lam2Bruijn
  ) where

import Control.Monad.Except
import qualified Data.Map as M
import Data.Char

import Name
import Environment
import Lambda

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

bruijn2Lam :: BruijnTerm i -> Either (UndefinedVar i Bound) (LamTerm i Name)
bruijn2Lam t = go t []
  where go :: BruijnTerm i -> [Name] -> Either (UndefinedVar i Bound) (LamTerm i Name)
        go (Var info n) env = case getAt env (toInt n ) of
            Nothing -> throwError $ UndefinedVar info n
            Just name -> return $ Var info name
        go (Val i v) _ = return $ Val i v
        go (Appl i e1 e2 ) env = Appl i <$> go e1 env <*> go e2 env
        go (Lambda info (Name n) e1) env =
            -- first name in [name,name1,name2..] that is not allready used
            let name = head $ dropWhile (`elem` env)
                    (map (\ i -> Name (n ++ i)) ("" : map show [(0 :: Int) ..] ))
            in Lambda info name <$> go e1 (name : env)

-- | Converts 'Lambda' with named variabls to 'Lambda' with Bruijn Index's
--
-- This function fails with 'Left' 'UndefinedVar' if there is a free variable.
--
-- It remove the number at the and of names ("name1"->"name") so: @lam2Bruijn bruijn2Lam == id @

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

-- TODO move to a utility module, or replace with libary function
getAt :: [a] -> Int -> Maybe a
getAt [] _ = Nothing
getAt (x : _) 0 = Just x
getAt (_ : xs) n = getAt xs (n - 1)
