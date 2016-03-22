module BackList where

import Control.Monad
import Control.Applicative

data BackList a = Failures !Int
                | List  [a] (Maybe (BackList a))
                deriving Show

instance Functor BackList where
  fmap = liftM

instance Applicative BackList where
  pure = return
  (<*>) = ap

instance Monad BackList where
    return a = List [a] Nothing
    Failures i >>= _ = Failures i
    -- List [] >>= _ = error "emty backlist"
    List (l:ls) next >>= f = setTop (Just (List ls next>>= f)) (f l)
    List [] (Just next) >>= f = next >>= f
    List [] Nothing >>= _ = Failures 0

--  TODO remove is not mplus
instance MonadPlus BackList where
    mzero = Failures 1
    mplus (Failures i ) (Failures j ) = Failures $! i + j
    mplus Failures {} b = b
    mplus (List l next) b = List (l ++ trys b) next

setTop :: Maybe(BackList a) -> BackList a -> BackList a
setTop (Just a)(Failures i) = mplus (Failures i) a
setTop Nothing (Failures i) = Failures i
setTop newtop (List t Nothing ) = List t newtop
setTop newtop (List t (Just oldtop)) = List t $ Just $  setTop newtop oldtop

instance Alternative BackList where
   empty = mzero
   (<|>) = mplus

try :: [a] -> BackList a
try [] = Failures 1
try as = List as Nothing

tryM :: [BackList a] -> BackList a
tryM [] = Failures 1
tryM list = foldr1 mplus list

trys  :: BackList a -> [a]
trys (Failures _) = []
trys (List a _) = a

toList :: BackList a -> [a]
toList Failures {} = []
toList (List l b) = l ++ concatMap toList b

failures :: BackList a -> Int
failures List {} = 0
failures (Failures i) = i
