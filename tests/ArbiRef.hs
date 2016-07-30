 {-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances #-}
module ArbiRef where

import Logic
import Generator

import BruijnEnvironment
import FreeEnvironment
import Name

class ArbiRef n where
    emptyStore :: VariableStore n

instance ArbiRef Bound where
    emptyStore =VS $ BS bEmtyEnv

instance ArbiRef Name where
    emptyStore =VS $ NS []

class Store f a where
    listNames :: f a -> [Name]
    insertVariable :: f a -> Bool -> Name -> Free -> f a
    findVariable :: f a -> Generater (a, Free)

data VariableStore a = forall f . Store f a => VS (f a)

instance Store VariableStore a where
    listNames (VS s) = listNames s
    insertVariable (VS s) b str f  = VS $ insertVariable s b str f
    findVariable (VS s) = findVariable s

newtype BoundStore a = BS { bStore:: BruijnEnv (Name, Free)}

instance Store BoundStore Bound where
    listNames s = map (fst . snd) $ bToList $ bStore s
    insertVariable = insertBound
    findVariable = findBound

-- TODO decouple bruijnMap
findBound :: BoundStore Bound -> Generater (Bound, Free )
findBound s = do
   (i, (_, f)) <- elementsLogic $ bToList $ bStore s
   return (Bound i, f)

insertBound :: BoundStore n -> Bool -> Name-> Free -> BoundStore n
insertBound store _ name free =BS newStore
    where newStore = bInsert (name, free) (bStore store)

newtype NameStore a = NS{nStore::[(Name ,Free)] }

instance Store NameStore Name where
    listNames s = map fst $ nStore s
    insertVariable = insertName
    findVariable = findName

findName :: NameStore Name -> Generater (Name, Free )
findName s = elementsLogic $ nStore s

insertName :: NameStore Name -> Bool -> Name -> Free -> NameStore Name
insertName (NS store ) newVar name free = NS newStore
    where newStore:: [(Name , Free)]
          newStore= (name, free) : (if newVar
            then removeVar name store
            else store)

-- removes the first occurrence of string with name (not all)
removeVar :: Name -> [(Name, b)] -> [(Name, b)]
removeVar varname env =
    let (left, right ) = span (\a -> fst a == varname) env
    in left ++ right
