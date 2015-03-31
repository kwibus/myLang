module BruijnTerm where
import Names
import Control.Monad.Error
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Exception.Base
import Control.Applicative
import Enviroment
import Lambda

type BruijnTerm i = LamTerm i Bound

toList :: BruiEnv a -> [(Int, a)]
toList BruiState {bruiMap = m} = IM.toList m

data UndefinedVar i = UndefinedVar i Name
    deriving (Show, Eq)

lam2Bruijn :: LamTerm i Name -> Either (UndefinedVar i) (BruijnTerm i)
lam2Bruijn t = go t 0 M.empty
  where go (Var i n) depth env = case M.lookup n env of
            Just n' -> return $ Var i $ Bound (depth - n' - 1)
            Nothing -> throwError $ UndefinedVar i n
        go (Val i v) _ _ = return $ Val i v
        go (Lambda i n t1) depth env = Lambda i n <$>
                     go t1 (depth + 1) (M.insert n depth env)
        go (Appl i t1 t2) depth env = Appl i <$> (go t1 depth env) <*> (go t2 depth env)


bSupsitute :: Free -> a -> BruiEnv a -> BruiEnv a
bSupsitute (Free i) a env@BruiState {bruiMap = m} =
    assert (IM.member (-i) m ) $ env {bruiMap = IM.insert (-i) a m}

bruijn2Lam :: BruijnTerm i -> LamTerm i Name
bruijn2Lam t = go t bEmtyEnv
  where go :: BruijnTerm i -> BruiEnv Name -> (LamTerm i Name)
        go (Var i n) env = Var i $ if bMember n env
                                    then bLookup n env
                                    else error $ "varibale " ++
                                           show n ++ "is used before defined"
        go (Val i v) _ = Val i v
        go (Appl i e1 e2 ) env = Appl i (go e1 env ) (go e2 env )
        go (Lambda i n e1) env = let (nenv, _) = bInsert n env
                               in Lambda i n (go e1 nenv)
