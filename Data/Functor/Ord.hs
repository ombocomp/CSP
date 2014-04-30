-- |Restricted variant of Functor whose elements
--  have an Ord-context. This allows additional types
--  (like Sets) to be instances. The laws for OrdFunctor
--  are the same as those for Functor.
--
--  Unlike Functor, OrdFunctor has no straightforward
--  generalisation to (Ord)Applcative, because OrdApplicative.<*>
--  would require a non-existent Ord (a -> b) instance.
--
--  For details about the functions here, see the module @Data.Functor@.
module Data.Functor.Ord where

import Prelude hiding (Functor(..), fmap, Monad (..))
import qualified Data.Functor as Fu (Functor(fmap))
import qualified Data.Set as Set

import Text.ParserCombinators.ReadP(ReadP)
import Text.ParserCombinators.ReadPrec(ReadPrec)
import GHC.Conc(STM)
import Control.Applicative(ZipList)

infixl 4 <$
infixl 4 <$>

class OrdFunctor f where
   fmap :: (Ord a, Ord b) => (a -> b) -> f a -> f b
   (<$) :: (Ord a, Ord b) => a -> f b -> f a
   x <$ f = fmap (const x) f

(<$>) :: (OrdFunctor f, Ord a, Ord b) => (a -> b) -> f a -> f b
(<$>) = fmap

instance OrdFunctor [] where fmap = Fu.fmap
instance OrdFunctor IO where fmap = Fu.fmap
instance OrdFunctor Maybe where fmap = Fu.fmap
instance OrdFunctor ReadP where fmap = Fu.fmap
instance OrdFunctor ReadPrec where fmap = Fu.fmap
instance OrdFunctor STM where fmap = Fu.fmap
instance OrdFunctor ZipList where fmap = Fu.fmap
instance OrdFunctor Set.Set where fmap = Set.map
