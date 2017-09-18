
{-# LANGUAGE FlexibleContexts #-}
module Step where
import Prelude hiding (map)
import qualified Prelude
import Data.Foldable
import Data.Bifunctor
import Control.Monad.Writer.Lazy

-- | 'Step' is used do incremental calculated something
-- intermediate valuse are writen out
type Step w a = Writer [w] a

runStep :: Step w a -> (a,[w])
runStep = runWriter

yield :: a -> Step a ()
yield a = tell [a]

each :: Foldable f => f w -> Step w ()
each = tell.toList

map :: (w1 -> w2) ->  Step w1 a -> Step w2 a
map f = mapWriterT (fmap $ second (Prelude.map f))

censors ::  MonadWriter [w] m => (w->w) -> m a -> m a
censors f = censor (Prelude.map f)
