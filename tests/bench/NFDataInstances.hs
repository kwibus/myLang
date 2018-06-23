{-# LANGUAGE FlexibleInstances, StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NFDataInstances where

import Control.DeepSeq
import Control.DeepSeq.Generics
import GHC.Generics hiding (Fixity,Associativity)
import Control.Monad.Trans.State
import Data.Functor.Identity
import Text.Parsec.Error

import Lambda
import Parser
import Name
import Value
import Type
import FreeEnvironment
import Associativity
import InfixFix
import SourcePostion
import BruijnTerm
import TypeError
import ErrorCollector

deriving instance  Generic Name
instance NFData Name where
    rnf = genericRnf

deriving instance  Generic (TypeA i)
instance NFData i => NFData (TypeA i) where
    rnf = genericRnf

deriving instance  Generic (Def i n)
instance (NFData i, NFData n) => NFData (Def i n) where
    rnf = genericRnf

deriving instance  Generic Free
instance NFData Free where
    rnf = genericRnf

deriving instance  Generic Bound
instance NFData Bound where
    rnf = genericRnf

deriving instance  Generic (TypeError i j)
instance (NFData i, NFData j) => NFData (TypeError i j)where
    rnf = genericRnf

deriving instance  Generic (UndefinedVar i b)
instance (NFData i, NFData b) => NFData (UndefinedVar i b)where
    rnf = genericRnf

deriving instance  Generic UnificationError
instance NFData UnificationError where
    rnf = genericRnf

deriving instance  Generic (ErrorCollector e a)
instance (NFData e , NFData a) =>NFData (ErrorCollector e a) where
    rnf = genericRnf

deriving instance  Generic Fixity
instance NFData Fixity where
    rnf = genericRnf

deriving instance  Generic Associativity
instance NFData Associativity where
    rnf = genericRnf

deriving instance  Generic (StateT  s m a)
instance  NFData (Control.Monad.Trans.State.StateT Stack Identity Value ) where
    rnf = genericRnf

deriving instance  Generic TypeInstance
instance NFData TypeInstance where
    rnf = genericRnf

deriving instance  Generic  Parser.ParseError
instance NFData Parser.ParseError where
    rnf =  genericRnf

deriving instance  Generic  InfixError
instance NFData InfixError  where
    rnf = genericRnf

-- deriving instance  Generic Text.Parsec.Pos.SourcePos
instance NFData SourcePos where
    rnf = flip seq ()

-- deriving instance  Generic Text.Parsec.Error.ParseError
instance NFData Text.Parsec.Error.ParseError where
    rnf = flip seq ()

deriving instance  Generic Value
instance NFData Value where
    rnf v = seq v ()

deriving instance  Generic (LamTerm i j n )
instance  (NFData i, NFData j, NFData n) => NFData (LamTerm i j n) where
    rnf = genericRnf
