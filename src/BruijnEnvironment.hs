module BruijnEnvironment where

import Data.Maybe
import Data.Bifunctor

-- TODO remove b prefix
-- | Bound is wrapper arround Int and is used to represent BruijnIndex.
-- BruijnIndex rever to a env, but are dependent on the surrounding terms.
-- If you add extra lambda:
--
-- * \\a.a  ==> \\a.\\b.a
--
-- * \\0  ==>   \\1
--
-- You have to modify the Inde
newtype Bound = Bound Int deriving (Eq, Show)

-- TODO embed length of scopes
newtype BruijnEnv a = BEnv[[a]] deriving (Show, Eq)

toInt :: Bound -> Int
toInt (Bound i) = i

bNull :: BruijnEnv a -> Bool
bNull (BEnv []) = True
bNull (BEnv [[]]) = True
bNull _ = False

bEmtyEnv :: BruijnEnv a
bEmtyEnv = BEnv []

bMember :: Bound -> BruijnEnv a -> Bool
bMember b e= isJust $ bMaybeLookup b e

bMaybeLookup :: Bound -> BruijnEnv a -> Maybe a
bMaybeLookup  (Bound i) (BEnv env)= go i env
  where
    go _ [] = Nothing
    go n (list:rest)  = case eitherAtIndexorLength n list of
       Right a -> Just a
       Left l -> go (n-l) rest

eitherAtIndexorLength :: Int -> [a] -> Either Int a
eitherAtIndexorLength i = go 0
  where
    go n [] = Left n
    go n (a:as) =
      if n == i
        then Right a
        else go (n+1) as

eitherReplaceAtIndexorLength :: Int -> a -> [a] -> Either Int [a]
eitherReplaceAtIndexorLength i new = go 0
  where
    go n [] = Left n
    go n (a:as) =
      if n == i
        then Right (new : as)
        else (a:) <$> go (n+1) as

bLookup :: Bound -> BruijnEnv a -> a
bLookup b env = case bMaybeLookup b env of
    Just a -> a
    Nothing -> error "not in env "

bInsert :: a -> BruijnEnv a -> BruijnEnv a
bInsert a (BEnv env) =  BEnv ([a] : env)

bInserts :: [a] ->  BruijnEnv a -> BruijnEnv a
bInserts a (BEnv env) =  BEnv (a : env)

bFromList :: [[a]] -> BruijnEnv a
bFromList = BEnv

bAppend :: BruijnEnv a -> BruijnEnv a -> BruijnEnv a
bAppend (BEnv a) (BEnv b) = BEnv (a ++ b)

bList :: BruijnEnv a -> [a]
bList (BEnv env) = concat env

bToList :: BruijnEnv a -> [(Int,a)]
bToList = bList . mapWithBound (\(Bound i) a -> (i,a))

bReplace :: Bound -> a -> BruijnEnv a -> BruijnEnv a
bReplace (Bound n) new (BEnv env) = BEnv $ go n env
  where
    -- go :: Int -> [[a]] -> [[a]]
    go _ [] = error "replace no exisiting variable"
    go i (a:as) = case eitherReplaceAtIndexorLength i new a of
        Left l-> (a:) $ go (i - l) as
        Right newa -> newa: as

-- TODO ??? could remove duplecate cate by using bSplitAt
bDrop :: Bound -> BruijnEnv a -> BruijnEnv a
bDrop n b = snd $ bSplitAt n b

bDropLevel :: BruijnEnv a -> BruijnEnv a
bDropLevel (BEnv scope) = BEnv $ drop 1 scope
--
-- TODO use bound ?
-- TODO rename this name is misleading
bSplitAt :: Bound -> BruijnEnv  a -> (BruijnEnv a, BruijnEnv a)
bSplitAt (Bound n) (BEnv env) = (BEnv finalLeft,BEnv finalRight)
  where
    (finalLeft,finalRight) = first reverse $ go [] n env
    go left _ [] = (left,[])
    go left i (current: right) =
        let l= length current
        in if l> i
           then  (left,current:right)
           else go (current:left) (i - l) right

instance Functor BruijnEnv  where
    fmap f (BEnv env) = BEnv $ map (map f) env

mapWithBound :: (Bound -> a -> b) -> BruijnEnv a -> BruijnEnv b
mapWithBound f (BEnv env) = BEnv $ mapWithBoundListList 0 env
 where mapWithBoundListList _ [] = []
       mapWithBoundListList i (a:as) = mapWithBoundList i a : mapWithBoundListList (length a + i) as
       mapWithBoundList i = zipWith f (map Bound [i..])

