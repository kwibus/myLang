{-# LANGUAGE MonoLocalBinds#-}
-- | this module defines sortTerm, that reorder let in a way the can be easly evaluated from top to bottum
module TopologicalSort
    ( topologicalSort
    , sortTerm
    , freevars
    , DataCycle (..)
    ) where

import Data.List (sortBy)
import Data.Ord
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bifunctor
import ModificationTags
import qualified TaggedLambda as Tag (LamTerm(..),unwrap)
import BruijnEnvironment
import BruijnTerm (BruijnTerm (),defsBounds)
import Lambda (Def (..),implementation)
import qualified Lambda as Lam
import LambdaF
-- TODO replace BruijnTerm with list Defs
-- | data type when sortTerm fails, cointaining :
--
-- * The Let defenitions where  cycle ocure
-- * the chain of dependencies that that let to cycle (exampel [Bound 0, Bound 1, Bound 0])
data DataCycle i = DataCycle (BruijnTerm i) [Bound] deriving (Eq, Show)

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
updateDepthM :: Monad m => Int -> BruijnTerm i -> m Int
updateDepthM i t = return $ updateDepth i t

updateDepth :: Int -> BruijnTerm i -> Int
updateDepth i Lam.Lambda {} = i+1
updateDepth i (Lam.Let _ defs _) = i+length defs
updateDepth i _ = i

freevars :: BruijnTerm i -> [Bound]
freevars = freeVarsToList 0 . bottumUpWith updateDepth (\depth _ astF -> updateFreeVars depth astF)0

updateFreeVars :: Int -> LamTermF i Bound FreeVars -> FreeVars
updateFreeVars  _ ValF {} = Set.empty
updateFreeVars depth (VarF _ b) = singlton depth b
updateFreeVars depth (LambdaF _ _ t) = removeOutScope (depth-1) t -- TODO maybe it better to keep out of scoop and down show them when queryed
updateFreeVars _ (ApplF free1 free2) = Set.union free1 free2
updateFreeVars depth (LetF _ defs freeT) = removeOutScope (depth-length defs) $ mconcat (freeT:map implementation defs)

sortTerm :: BruijnTerm i -> Either (DataCycle i) (LamTerm i )
sortTerm term = snd <$> bottumUpWithM updateDepthM go 0 term
  where
    go :: Int -> BruijnTerm i -> LamTermF i Bound (FreeVars, LamTerm i) -> Either (DataCycle i) (FreeVars, LamTerm i)
    go depth orignal (LetF i defs (freeT,t)) =
        let oldDepth = depth - length defs
            (freeDefs,defs',depencys) = unzip3 $ map  ( \(b, Lam.Def i_ n_ (freeIndDef, newDef)) ->
                let (free, boundLet) = splitPast oldDepth freeIndDef
                    depency = (b,freeVarsToList depth boundLet)
                in (free,Def i_ n_ newDef,depency)
              ) (zip (defsBounds defs) defs)
            (funcDef, valDep) = partitionWith (isFunction .snd. Lam.implementation) depencys defs
        in case  topologicalSort valDep funcDef of
          Left dependencyChain -> Left $ makeDataCycle orignal dependencyChain
          Right newOrder ->
            let reorderTerm = Tag.Tag $ Reorder 0 $ order2reorder newOrder --TODO replace with funciton
                sortedDefs = map (fmap reorderTerm . (\ b -> bLookup b $ bFromList defs')) newOrder
            in return (mconcat $ removeOutScope oldDepth freeT : freeDefs
                   ,Tag.Let i sortedDefs $ reorderTerm t)

    go depth _ ast = return (updateFreeVars depth $ fmap fst ast ,Tag.unwrap $ fmap snd ast)
    -- go _ (ValF i v) = return (Set.empty,Tag.Val i v)
    -- go depth (VarF i b) = return (singlton depth b, Tag.Var i b)
    -- go _ (LambdaF i n t) = return $ second (Tag.Lambda i n) t
    -- go _ (ApplF (free1,t1) (free2, t2)) = return (Set.union free1 free2 , Tag.Appl t1 t2)

-- TODO define in terms of Eval.BruijnTerm2DenotionalValue
-- could define "Let defs fucntion" also to be a fucntion. but this would meen you have reevaluate defs every time you call the function
--
-- TODO move to other module
-- | is used to deterime if the term is allowed to depend on its self
isFunction :: LamTerm i -> Bool
isFunction term = case term of
        Tag.Lambda {} -> True
        Tag.Tag _ t -> isFunction t
        _ -> False

-- | given current depth add bruijen variable to set of freevarabls
insert :: Int -> Bound -> FreeVars -> FreeVars
-- store depth-defined = current depth - bruijn-index (how much higer is defined)
insert depth (Bound b) = Set.insert (depth - b)

singlton :: Int -> Bound -> FreeVars
singlton depth b = insert depth b Set.empty

-- | maps gives a list bruijnen index of the freevarible
freeVarsToList :: Int -> FreeVars -> [Bound]
freeVarsToList depth vars = map (Bound . (depth -)) $ Set.toList vars

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

makeDataCycle :: BruijnTerm i -> [Bound] -> DataCycle i
makeDataCycle term chain = DataCycle term $ dropWhile (/= last chain ) chain
