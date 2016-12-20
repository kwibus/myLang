module PrintBruijn where

import PrettyPrint
import BruijnTerm

printBrujin :: BruijnTerm () -> String
printBrujin = either show PrettyPrint.pShow . bruijn2Lam
