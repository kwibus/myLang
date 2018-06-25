module Info  where

import Lambda

removeInfo :: LamTerm i j n -> LamTerm () () n
removeInfo = mapJ (const ()) .mapI (const ())

mapI ::  (i1 -> i2) -> LamTerm i1 j n -> LamTerm i2 j n
mapI f (Appl e1 e2) = Appl (mapI f e1) (mapI f e2)
mapI _ (Val j v) = Val j v
mapI f (Lambda i n e) = Lambda (f i) n $ mapI f e
mapI _ (Var j n) = Var j n
mapI f (Let j defs e) = Let j (map (fmap (mapI f) . mapIDef f)defs) $ mapI f e

mapIDef :: (i1 -> i2) -> Def i1 t -> Def i2 t
mapIDef f (Def i n e1) = Def (f i) n e1

mapJ ::  (j1 -> j2) -> LamTerm i j1 n -> LamTerm i j2 n
mapJ f (Appl e1 e2) = Appl (mapJ f e1) (mapJ f e2)
mapJ f (Val j v) = Val (f j) v
mapJ f (Lambda i n e) = Lambda i n $ mapJ f e
mapJ f (Var j n) = Var (f j) n
mapJ f (Let j defs e) = Let (f j) (map (fmap (mapJ f)) defs) $ mapJ f e
