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
import LambdaF

data LamTerm i j n t = Lambda i Name (LamTerm i j n t)
            | Appl (LamTerm i j n t) (LamTerm i j n t)
            | Var j n
            | Val j Value
            | Let j [Def i (LamTerm i j n t)] (LamTerm i j n t)
            | Tag t (LamTerm i j n t)
            deriving (Eq, Show)

tag :: Lam.LamTerm i j n -> LamTerm i j n t
tag = unsafeCoerce
-- tag (Lam.Lambda i n t) = Lambda i n $ tag t
-- tag (Lam.Appl t1 t2)  = Appl (tag t1)(tag t2)
-- tag (Lam.Var i n) = Var i n
-- tag (Lam.Val i v)  = Val i v
-- tag (Lam.Let i defs t ) = Let i (map tagDef defs) $ tag t
--   where
--     tagDef (Lam.Def  i' n' t')  = Def i' n' $ tag t'

unwrap :: LamTermF i j n (LamTerm i j n t) -> LamTerm i j n t
unwrap (VarF i n) = Var i n
unwrap (ValF i v) = Val i v
unwrap (LambdaF i n t) = Lambda i n t
unwrap (ApplF t1 t2) = Appl t1 t2
unwrap (LetF i defs t) = Let i defs t

getVal :: LamTerm i j n t-> Maybe Value
getVal (Tag _ t) = getVal t
getVal (Val _ v) = Just v
getVal _ = Nothing
