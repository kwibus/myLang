module TaggedLambda
  ( module TaggedLambda
  ,Def (..)
  )
where

import Name
import Value
import qualified Lambda as Lam
import Unsafe.Coerce
import Lambda (Def(..))
data LamTerm i n t = Lambda i Name (LamTerm i n t)
            | Appl (LamTerm i n t) (LamTerm i n t)
            | Var i n
            | Val i Value
            | Let i [Def i (LamTerm i n t)] (LamTerm i n t)
            | Tag t (LamTerm i n t)
            deriving (Eq, Show)

tag :: Lam.LamTerm i n -> LamTerm i n t
tag = unsafeCoerce
-- tag (Lam.Lambda i n t) = Lambda i n $ tag t
-- tag (Lam.Appl t1 t2)  = Appl (tag t1)(tag t2)
-- tag (Lam.Var i n) = Var i n
-- tag (Lam.Val i v)  = Val i v
-- tag (Lam.Let i defs t ) = Let i (map tagDef defs) $ tag t
--   where
--     tagDef (Lam.Def  i' n' t')  = Def i' n' $ tag t'

getVal :: LamTerm i n t-> Maybe Value
getVal (Tag _ t) = getVal t
getVal (Val _ v) = Just v
getVal _ = Nothing
