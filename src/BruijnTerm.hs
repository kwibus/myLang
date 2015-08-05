module BruijnTerm where

import Control.Monad.Except
import qualified Data.Map as M
import Data.Char

import Name
import Environment
import Lambda

type BruijnTerm i = LamTerm i Bound

data UndefinedVar i n = UndefinedVar i n
    deriving (Show, Eq)

lam2Bruijn :: LamTerm i Name -> Either (UndefinedVar i Name ) (BruijnTerm i)
lam2Bruijn t = go t 0 M.empty
  where removeIndex n = Name $ takeWhile (not . isDigit) n
        go :: LamTerm i Name -> Int -> M.Map Name Int -> Either (UndefinedVar i Name ) (BruijnTerm i)
        go (Var i n) depth env = case M.lookup n env of
            Just n' -> return $ Var i $ Bound (depth - n' - 1)
            Nothing -> throwError $ UndefinedVar i n
        go (Val i v) _ _ = return $ Val i v
        go (Lambda i (Name n) t1) depth env =
                 Lambda i (removeIndex n) <$> go t1 (depth + 1) (M.insert (Name n) depth env)
        go (Appl i t1 t2) depth env = Appl i <$> go t1 depth env <*> go t2 depth env

bruijn2Lam :: BruijnTerm i -> Either (UndefinedVar i Bound) (LamTerm i Name)
bruijn2Lam t = go t []
  where go :: BruijnTerm i -> [Name] -> Either (UndefinedVar i Bound) (LamTerm i Name)
        go (Var info n) env = case getAt env (toInt n ) of
            Nothing -> throwError $ UndefinedVar info n
            Just name -> return $ Var info name
        go (Val i v) _ = return $ Val i v
        go (Appl i e1 e2 ) env = Appl i <$> go e1 env <*> go e2 env
        go (Lambda info (Name n) e1) env =
            let name = head $ dropWhile (`elem` env)
                    (map (\ i -> Name (n ++ i)) ("" : map show [(0 :: Int) ..] ))
            in Lambda info name <$> go e1 (name : env)

getAt :: [a] -> Int -> Maybe a
getAt [] _ = Nothing
getAt (x : _) 0 = Just x
getAt (_ : xs) n = getAt xs (n - 1)
