module ExampleBruijn where

import BruijnTerm
import Vallue
import Lambda

omega :: BruijnTerm Vallue
omega = Lambda "a" $ Appl (Var 0) (Var 0)

id :: BruijnTerm Vallue
id = Lambda "a" $ Var 0

s :: BruijnTerm Vallue
s = Lambda "x" (Lambda "y" (Var 1))
