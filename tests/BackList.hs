module BackList where

import Control.Monad
import Control.Applicative

data BackList a = Failures !Int
                | List  [a] (Maybe (BackList a))
                | StepBacks Int
                | Save (BackList a -> BackList a)
                -- deriving Show

instance Functor BackList where
  fmap = liftM

instance Applicative BackList where
  pure = return
  (<*>) = ap

instance Monad BackList where
    return a = List [a] Nothing
    Save c >>= f = Save $ f . c
    Failures i >>= _ = Failures i
    -- List [] >>= _ = error "emty backlist"
    List l next >>= f = setTop ((>>= f)<$>next) $ tryM (map f l) -- setTop (Just (List ls next>>= f)) (f l)
    List [] (Just next) >>= f = next >>= f
    List [] Nothing >>= _ = Failures 0

mkSavePoint :: BackList a
mkSavePoint = Save save

save :: BackList a -> BackList a
save (List (l:ls) next) = List [l] (Just (List ls  next))
save a = a

--  TODO remove is not mplus
instance MonadPlus BackList where
    mzero = Failures 1
    mplus (Failures i ) (Failures j ) = Failures $! i + j
    mplus Failures {} b = b
    mplus (List l next) b = List (l ++ trys b) next

setTop :: Maybe(BackList a) -> BackList a -> BackList a
setTop (Just a) (Save f)  = f a
setTop Nothing (Save f)  = Save f
setTop newtop (List t Nothing ) = List t newtop
setTop newtop (List t (Just oldtop)) = List t $ Just $  setTop newtop oldtop
setTop (Just a)(Failures i) = mplus (Failures i) a
setTop Nothing (Failures i) = Failures i

instance Alternative BackList where
   empty = mzero
   (<|>) = mplus

try :: [a] -> BackList a
try [] = Failures 1
try as = List as Nothing

tryM :: [BackList a] -> BackList a
tryM list = foldl1 mplus list

trys  :: BackList a -> [a]
trys (Failures _) = []
trys (List a _) = a

toList :: BackList a -> [a]
toList Failures {} = []
toList (List l b) = l ++ concatMap toList b

failures :: BackList a -> Int
failures List {} = 0
failures (Failures i) = i
