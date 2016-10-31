module BruijnEnvironment where
import Data.List (foldl')
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
    Nothing -> error $ show b ++ " not in env"

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

matchLevels :: BruijnEnv a -> BruijnEnv a -> BruijnEnv a
matchLevels env1 env2 =
    let diff = levels env2 - levels env1
    in if diff >= 0
       then iterate bDropLevel env2  !! diff
       else env2

levels :: BruijnEnv a -> Int
levels (BEnv env) = length env

getDepth :: Bound -> BruijnEnv a -> Int
getDepth (Bound i) (BEnv env) = go 0 env
 where go depth (l:ls) = let newdepth = depth + length l
                         in if newdepth > i
                            then depth
                            else go newdepth ls
       go _ [] = error "not in scope"

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

instance Monoid (BruijnEnv a) where
    mempty = BEnv []
    mappend = bAppend

bfoldl  :: (b -> a -> b) -> b -> BruijnEnv a -> b
bfoldl f b (BEnv env) =  foldl (foldl f) b env

transforml :: (b -> a ->  (b,c)) -> b -> BruijnEnv a  -> (b,BruijnEnv c)
transforml f b0 (BEnv env) = go (b0,[]) env
  where go (b,prev) [] = (b,BEnv $reverse prev)
        go (b,prev) (l:ls) = let (newB,newL) = reform b l
                      in  go (newB, newL: prev) ls
        -- reform :: b->[a] -> (b,[a])
        reform b list = second reverse $ foldl' reform1 (b,[]) list
        -- reform1 :: (b,[c] -> [c]) -> a -> (b,[c] -> [c])
        reform1 (b,prev) a=
              let (newb, c) = f b a
              in (newb, c:prev)

mapWithBound :: (Bound -> a -> b) -> BruijnEnv a -> BruijnEnv b
mapWithBound f (BEnv env) = BEnv $ mapWithBoundListList 0 env
 where mapWithBoundListList _ [] = []
       mapWithBoundListList i (a:as) = mapWithBoundList i a : mapWithBoundListList (length a + i) as
       mapWithBoundList i = zipWith f (map Bound [i..])

mapLevel :: Int -> (a -> a) -> BruijnEnv a -> BruijnEnv a
mapLevel level f (BEnv env) = BEnv $ modify (length env - level) (map f) env

modify :: Int -> (a -> a)  -> [a] -> [a]
modify index f list = left ++ [f x] ++ right
    where (left,x:right)= splitAt (index-1) list
