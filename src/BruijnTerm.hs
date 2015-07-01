module BruijnTerm where

import Control.Monad.Except
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Names
import Enviroment
import Lambda

type BruijnTerm i = LamTerm i Bound

toList :: BruiEnv a -> [(Int, a)]
toList BruiState {bruiMap = m} = IM.toList m

data UndefinedVar i n = UndefinedVar i n | RefShadow i Bound
    deriving (Show, Eq)

lam2Bruijn :: LamTerm i Name -> Either (UndefinedVar i Name ) (BruijnTerm i)
lam2Bruijn t = go t 0 M.empty
  where go :: LamTerm i Name -> Int -> M.Map Name Int -> Either (UndefinedVar i Name ) (BruijnTerm i)
        go (Var i n) depth env = case M.lookup n env of
            Just n' -> return $ Var i $ Bound (depth - n' - 1)
            Nothing -> throwError $ UndefinedVar i n
        go (Val i v) _ _ = return $ Val i v
        go (Lambda i n t1) depth env = Lambda i n <$>
                     go t1 (depth + 1) (M.insert n depth env)
        go (Appl i t1 t2) depth env = Appl i <$> (go t1 depth env) <*> (go t2 depth env)

bruijn2Lam :: BruijnTerm i -> Either (UndefinedVar i Bound) (LamTerm i Name)
bruijn2Lam t = go t []
  where go :: BruijnTerm i -> [Name] -> Either (UndefinedVar i Bound) (LamTerm i Name)
        go (Var i n) env = case splitAt (toInt n ) env of
            (_ , []) -> throwError $ UndefinedVar i n
            (lowerScoped, (name : higerscoped)) ->
                if elem name lowerScoped
                then throwError $ RefShadow i n
                else return $ Var i name
        go (Val i v) _ = return $ Val i v
        go (Appl i e1 e2 ) env = Appl i <$> go e1 env <*> go e2 env
        go (Lambda i n e1) env = Lambda i n <$> go e1 (n : env)
