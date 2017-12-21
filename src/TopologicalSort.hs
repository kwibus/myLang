{-# LANGUAGE MonoLocalBinds#-}
-- | this module defines sortTerm, that reorder let in a way the can be easly evaluated from top to bottum
module TopologicalSort
    ( topologicalSort
    , sortTerm
    , DataCycle (..)
    ) where

import Data.List (sortBy)
import Data.Ord
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bifunctor
import ModificationTags
import qualified TaggedLambda as Tag
import BruijnEnvironment
import BruijnTerm
import qualified Lambda as Lam

-- TODO replace BruijnTerm with list Defs
-- | data type when sortTerm fails, cointaining :
--
-- * The Let defenitions where  cycle ocure
-- * the chain of dependencies that that let to cycle (exampel [Bound 0, Bound 1, Bound 0])
data DataCycle i = DataCycle (BruijnTerm i i) [Bound] deriving (Eq, Show)

--TODO rename (current name refers how to i use it, no on how it can be used)
type FreeVars = Set.Set Int

-- | 'sortTerm' reorder let defenitions in topological order. you should be able to evaluated al expresion of let defenitions to normalform without needing not yet evaluated expresions.
--
-- * this order is not unique
--
-- * this function return 'Left' DataCycle' when this is not possible (@ let a = a in a @)
--
-- * this function does not rename variables (bruijen index), but add tages that descips how it should be renamed. this can be done with 'ModificationTags.applyModify'

-- TODO consider to add the ablity find more then one error
-- TODO check if it would be easyer to refere to let defenitions with [0..Ndefs-1] instead of [Ndefs-1 .. 0]
-- TODO split freevars accumulate and reorder Let
-- this function finds freevars of definition's. if one of this freevars refers to other definition's in let add it as depency.
-- if the defenition (normal form)/ function it may refer to its self
-- topologicalSort those decency's of the definition's
-- reorder definitions
-- tag definitions for renaming variable (you cant do it one pass, because freevars accumulation is bottom up and renaming is top down)
--
-- tricks used:
--      bruijen index depent on its distance from its defenition, so to store these while these distances are changing is hard.
--      the solution is to store distance from start of full lambda to depth of were variable is first defined
--      to get back the the orinal bruijen index:
--      bruijen-index = depth - depth-defined
--
--      topologicalSort expect normal naming scheme, no relative (bruij-index's)
--      but if you only work in fixed scope/depth the naming is fixed

sortTerm :: BruijnTerm i i -> Either (DataCycle i) (Tag.LamTerm i i Bound (Modify i))
sortTerm term = snd <$> go 0 term
  where
    -- could replace Either e (b,a).. with EitherT e (b,) a
    -- this would make is possible to use standard mapchild like function
    -- and would replace boilerplate
    go :: Int -> BruijnTerm i i-> Either (DataCycle i) (FreeVars, Tag.LamTerm i i Bound (Modify i))
    go _ (Lam.Val i v) = return (Set.empty,Tag.Val i v)
    go depth (Lam.Var i b) = return (insert depth b Set.empty, Tag.Var i b)
    go depth (Lam.Lambda i n t) = fmap (Tag.Lambda i n) <$> go (depth + 1) t
    go depth (Lam.Appl t1 t2) = do
        (free1 ,t1') <- go depth t1
        (free2, t2') <- go depth t2
        return (Set.union free1 free2, Tag.Appl t1' t2')
    go depth (Lam.Let i defs t) = do
        let ndefs = length defs
        let newDepth = depth + ndefs
        (freeT,t') <- go newDepth t
        (freeDefs,defs',depencys) <- unzip3 <$> mapM  ( \(b, Def i_ n_ t_) -> do
                (freeIndDef,newDef) <- go newDepth t_
                let (free, boundLet) = splitPast depth $ removeOutScope newDepth freeIndDef
                let depencys = (b,freeVarsToList newDepth boundLet)
                return (free,Def i_ n_ newDef,depencys)
              ) (zip (defsBounds defs) defs)
        -- here i use the old defs not the ones with already sorted  terms
        -- because it easyer to find out if its a fucntion
        -- so i use the assumpiton that go does not change if its a function
        let (funcDef, valDep) = partitionWith (isFunction . Lam.implementation) depencys defs
        newOrder <- first (makeDataCycle (Lam.Let i defs t)) $ topologicalSort valDep funcDef
        let reorderTerm = Tag.Tag $ Reorder 0 $ order2reorder newOrder --TODO replace with funciton
        let sortedDefs = map (fmap reorderTerm . (\ b -> bLookup b $ bFromList defs')) newOrder
        return (mconcat $ removeOutScope depth freeT : freeDefs
               ,Tag.Let i sortedDefs $ reorderTerm t')

-- TODO define in terms of Eval.BruijnTerm2DenotionalValue
-- could define "Let defs fucntion" also to be a fucntion. but this would meen you have reevaluate defs every time you call the function
--
-- | is used to deterime if the term is allowed to depend on its self
isFunction :: Lam.LamTerm i i n -> Bool
isFunction term = case term of
        Lam.Lambda {} -> True
        _ -> False

-- | given current depth add bruijen variable to set of freevarabls
insert :: Int -> Bound -> FreeVars -> FreeVars
-- store depth-defined = current depth - bruijn-index (how much higer is defined)
insert depth (Bound b) = Set.insert (depth - b)

-- | maps gives a list bruijnen index of the freevarible
freeVarsToList :: Int -> FreeVars -> [Bound]
freeVarsToList depth freeVars = map (Bound . (depth -)) $ Set.toList freeVars

-- | given current depth and freeVars its gives freevars that are inscope
removeOutScope :: Int -> FreeVars -> FreeVars
-- remove all variable that are defined deep then current depth
removeOutScope depth vars = fst $ Set.split (depth + 1) vars

-- | split from low to including a, and reset
splitPast :: Ord a => a -> Set.Set a -> (Set.Set a, Set.Set a)
splitPast pivot set =
    if member
    then (Set.insert pivot less, bigger)
    else (less, bigger)
  where
      (less, member, bigger) = Set.splitMember pivot set

-- TODO could keep orinal order
partitionWith :: (b -> Bool) -> [a] -> [b] -> ([a], [a])
partitionWith f as bs = foldl split ([], []) $ zip as bs
  where
    split (trues, falses) (a, b) =
        if f b
            then (a : trues,     falses)
            else (trues    , a : falses)

-- TODO maybe rename?
-- | when given new order in witch  bruijenvariable are defined it returns a list of subsitutions to correct subtems.
--
-- example:
--
-- >>> order2reorder [Bound 2, Bound 0 ,Bound 1]
-- [Bound 1,Bound 0,Bound 2]
--
-- meaning substituut [Bound 0 -> Bound 1, Bound 1 -> Bound 2, Bound 2]
-- this substitutions is  needed because Bound 0 refers to last defined argument. But that is now the second last defined argument
order2reorder:: [Bound] -> [Bound]
order2reorder order = map fst $ sortBy (comparing snd ) relation
  where
    -- (old, new) (reverse because bruijn-index 0 reverse to last defined)
    relation = zip (map Bound [0 ..]) $ reverse order

data Tag a = Processed | StrongDepencys [a] | WeakDepencys [a] | Forbidden deriving Show
type Tags a = Map.Map a (Tag a)

-- | 'topologicalSort' will make a list of a dependency graph. In this list all dependency will become before the things them self.
--
-- * program will give 'Left' depency-chain if there is a cyclic dependency, berceaus then there exist no solution
--
--      * the depency-chain is a list Nodes, path to the cycle, (backtrace), ending where cycle was detected.
--      * The first and second arguments are list of items/task with there dependence's.
--        The items/task of the second list may depend direct or indirect on itself without errors
--
--      * possible depency-chain: @[b,a,c,a]@
--
-- * if no dependency of a item/task are given, then no dependency's are assumed
--
-- example:
--
-- >>> :{
--      topologicalSort [("hotdog"      , ["dog","bun","mustard"])
--                      ,("bun"         , [])
--                      ,("dog"         , ["puppies"])
--                      ,("mustard"     , ["mustard seed","vinegar"])
--                      ]
--                      [("me",["me","hotdog"])]
--     :}
--Right ["puppies","dog","bun","mustard seed","vinegar","mustard","hotdog","me"]

-- TODO rename weak strong
-- TODO give meaning ful error messages
--
-- this function walks depency graph depth first
--
-- if node has no dependency it can be inserted directly
-- if node has dependency's mark is as Forbidden.
--      (if its children are its parent have a cycle, if its node witch is allowed refer to itself mark it instead as Processed)
--      do all of its depencys first
--      tag this node as Processed and insert node
-- if the node is tagged as Processed it`s already checked  and inserted so go to next
-- if visited node is tagged as forbidden its part fo cycle
-- if node is not tagged assume no depencys so inserted direcly

topologicalSort :: Ord a => [(a, [a])] -> [(a, [a])] -> Either [a] [a]
topologicalSort strong weak = reverse . snd <$> foldM visit (initTags, []) tasks
  where
    -- tasks :: [a]
    tasks = map fst strong ++ map fst weak
    -- initTags :: Tags a
    initTags = Map.fromList $ map (second StrongDepencys) strong ++ map (second WeakDepencys ) weak
    -- visit :: (Tags a, [a]) -> a ->Either [a] (Tags a, [a])
    visit (tags, order) a = case Map.lookup a tags of
        Just (StrongDepencys dependencys) -> do
            (newtags, neworder) <- first (a :) $ foldM visit (Map.insert a Forbidden tags, order) dependencys
            return (Map.insert a Processed newtags, a : neworder)
        Just (WeakDepencys dependencys) -> do
            (newtags, neworder) <- first (a :) $ foldM (weakvisit a) (tags, order) dependencys
            return (Map.insert a Processed newtags, a : neworder)
        Just Forbidden -> Left [a]
        Just Processed -> return (tags, order)
        Nothing -> return (tags, a : order)
    weakvisit :: Ord a => a -> (Tags a, [a]) -> a -> Either [a] (Tags a, [a])
    weakvisit parrent (tags, order) a = case Map.lookup a tags of
        Just (WeakDepencys _) -> visit (Map.insert parrent Processed tags, order) a
        Just (StrongDepencys _) -> visit (Map.insert parrent Forbidden tags, order) a
        _ -> visit (tags, order) a

makeDataCycle :: BruijnTerm i i -> [Bound] -> DataCycle i
makeDataCycle term chain = DataCycle term $ dropWhile (/= last chain ) chain
