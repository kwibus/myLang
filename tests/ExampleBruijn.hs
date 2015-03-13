module ExampleBruijn where

import BruijnTerm
import Vallue
import Lambda
import Enviroment

omega :: BruijnTerm Vallue
omega = Lambda "a" $ Appl (Var (Bound 0)) (Var (Bound 0))

id :: BruijnTerm Vallue
id = Lambda "a" $ Var (Bound 0)

s :: BruijnTerm Vallue
s = Lambda "x" (Lambda "y" (Var (Bound 1)))
