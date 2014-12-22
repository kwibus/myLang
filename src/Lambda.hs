module Lambda where
import Vallue

type Name = String

data LamTerm = Lambda Name LamTerm
            | Appl LamTerm LamTerm
            | Var Variable
            deriving (Eq, Show)

var :: Name -> LamTerm
var = Var . VarVar

val :: Vallue -> LamTerm
val = Var . Val

-- TODO rename VarVar
data Variable = VarVar Name | Val Vallue deriving (Eq, Show )

pShowVar :: Variable -> String
pShowVar (VarVar n) = n
pShowVar (Val v) = pShowVal v

pShowVal :: Vallue -> String
pShowVal (MyDouble a) = show a

pShowVal BuildIn {name = n} = n

pShow :: LamTerm -> String
pShow = go False where
      go _ (Var n) = pShowVar n
      go b (Lambda n t) = "\\" ++ n ++ "." ++ go b t
      go b (Appl t1@Lambda {} t2@Var {}) = parentheses t1 ++ go b t2
      go _ (Appl t1@Lambda {} t2) = parentheses t1 ++ parentheses t2
      go b (Appl t1@Var {} t2@Var {}) = go b t1 ++ " " ++ go b t2
      go b (Appl t1@Var {} t2) = go b t1 ++ parentheses t2
      go _ (Appl t1@Appl {} t2@Appl {}) = go True t1 ++ parentheses t2
      go True (Appl t1@Appl {} t2@Lambda {}) = go True t1 ++ parentheses t2
      go b (Appl t1@Appl {} t2@Var {}) = go True t1 ++ " " ++ go b t2
      go b (Appl t1@Appl {} t2 ) = go True t1 ++ go b t2

parentheses :: LamTerm -> String
parentheses s = "(" ++ pShow s ++ ")"
