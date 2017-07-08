module Inprocess where

import Data.Bifunctor
import MTable (MTable,empty)
import qualified ModifiedLambda

import qualified BruijnTerm as Lam
import qualified TaggedLambda as Tag
import ModificationTags
import BruijnEnvironment
import LambdaF
import Value
import Name

-- FIXME when you walk over tree you have to know wich part have already been modified and which not

data LamTermFT = App InProcess InProcess
               | Lambda Name InProcess
               | Let [ DefF () Bound InProcess ] InProcess
               | Tag (Modify ()) InProcess
               deriving Show

data InProcess = Inproc LamTermFT
              | Unproc (Tag.LamTerm () Bound (Modify()))
              | New (Tag.LamTerm () Bound (Modify()))
              deriving Show

addTag :: Modify () -> InProcess -> InProcess
addTag m (New t) = New $ Tag.Tag m t
addTag m (Unproc t) = Unproc $ Tag.Tag m t
addTag m (Inproc t) = Inproc $ Tag  m  $ Inproc t -- TODO maybe map over leaves

appl :: InProcess -> InProcess -> InProcess
appl (New t1) (New t2) = New $ Tag.Appl t1 t2
appl (Unproc t1) (Unproc t2) = Unproc $ Tag.Appl t1 t2
appl t1 t2 = Inproc $ App t1 t2

mkLet :: [DefF () Bound InProcess ] -> InProcess -> InProcess
mkLet defs t = Inproc $ Let defs t

var :: Bound -> InProcess
var b = New $ Tag.Var () b

val :: Value -> InProcess
val v = New $ Tag.Val () v

lambda :: Name -> InProcess -> InProcess
lambda n (New t) = New $ Tag.Lambda () n t
lambda n (Unproc t) = Unproc $ Tag.Lambda () n t
lambda n (Inproc t) = Inproc $ Lambda n $ Inproc t

-- TODO rename mod
peek :: MTable -> InProcess -> (LamTermF () Bound InProcess, MTable)
peek modification (Inproc term)= case term of
  (App t1 t2) -> (ApplF t1 t2,modification)
  (Lambda n t) -> (LambdaF () n t,modification)
  (Let defs t) -> (LetF () defs t,modification)
  Tag {}  ->error  "Doom"--FIXME
peek _ (New term) = peek empty $ Unproc term
peek modifications (Unproc term) =first (fmap Unproc) $  ModifiedLambda.peek modifications term

proces ::  MTable  -> InProcess -> Lam.BruijnTerm ()
proces = unfold peek

procesDef :: MTable-> DefF () Bound InProcess -> Lam.Def () Bound
procesDef modifications (DefF i n t) = Lam.Def i n (proces modifications t)
