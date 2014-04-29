-- |Module contains the @DataStructure@ and
--  @Retrievable@ typeclasses, together
--  with the @Stack@ and @Queue@ data types
--  which implement these.
module DataStructure (
   Stack,
   Queue,
   DataStructure(..),
   Retrievable(..),
   insertAll) where

import Control.Arrow
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Monoid as Mo
import qualified Data.Set as S
import Data.Set (Set)

newtype Stack a = Stk [a] deriving (Show, Eq, Ord, Read)

data Queue a = Queue [a] [a]

instance Show a => Show (Queue a) where
   show (Queue en de) = "Queue" ++ show (en ++ reverse de)

instance Eq a => Eq (Queue a) where
   (Queue en1 de1) == (Queue en2 de2) =
      (en1 ++ reverse de1) == (en2 ++ reverse de2)

instance Ord a => Ord (Queue a) where
   compare (Queue en1 de1) (Queue en2 de2) =
      compare (en1 ++ reverse de1) (en2 ++ reverse de2)

-- |Structures which offer the insertion and deletion
--  of elements, and the checking for membership.
class DataStructure d where
   -- |Create a new, empty data structure.
   new :: d a
   -- |Insert an element.
   insert :: (Eq a, Ord a) => a -> d a -> d a
   -- |Delete an element.
   delete :: (Eq a, Ord a) => a -> d a -> d a
   -- |Returns whether a value is contained in a data structure.
   elemOf :: (Eq a, Ord a) => a -> d a -> Bool

-- |Data structures which allow the sequential
--  retrieval (popping) of their elements.
--  Minimal complete definition: @pop@, @peek@.
class DataStructure d => Retrievable d where
   -- |Removes the first element from the data structure and
   --  returns it, together with the new structure.
   pop :: (Eq a, Ord a) => d a -> (a, d a)
   -- |Returns the first elements of the data structure, without
   --  modifying it.
   peek :: (Eq a, Ord a) => d a -> Maybe a
   -- |Returns the elements of the data structure, in @pop@-order.
   elems :: (Eq a, Ord a) => d a -> [a]
   elems = L.unfoldr pop'
      where pop' d = if isEmpty d then Nothing else Just $ pop d
   -- |Returns true iff the data structure has no elements.
   isEmpty :: (Eq a, Ord a) => d a -> Bool
   isEmpty = M.isNothing . peek

-- |Inserts all elements in a foldable container into a data structure.
insertAll :: (Eq a, Ord a, DataStructure d, Foldable f)
          => f a -> d a -> d a
insertAll = flip (F.foldl' (flip insert))

----------- Instances

------- Lists

instance DataStructure [] where
   new = []
   insert = (:)
   delete = L.delete
   elemOf = L.elem

instance Retrievable [] where
   pop = head &&& tail
   peek = M.listToMaybe
   elems = id

------- Sets

instance DataStructure Set where
   new = S.empty
   insert = S.insert
   delete = S.delete
   elemOf = S.member

instance Retrievable Set where
   peek = M.listToMaybe . S.toAscList
   pop = M.fromJust . peek &&& S.fromAscList . tail . S.toAscList

------- Stacks (lists)

instance DataStructure Stack where
   new = Stk new
   insert x (Stk xs) = Stk (x `insert` xs)
   delete x (Stk xs) = Stk (x `delete` xs)
   elemOf x (Stk xs) = x `elemOf` xs

instance Retrievable Stack where
   pop (Stk xs) = (head xs, Stk $ tail xs)
   peek (Stk []) = M.Nothing
   peek (Stk (x:_)) = M.Just x

instance Mo.Monoid (Stack a) where
   mempty = new
   mappend (Stk xs) (Stk ys) = Stk (xs ++ ys)

------- Queues

instance DataStructure Queue where
   new = Queue [] []
   insert x (Queue [] de) = Queue [x] de
   insert x (Queue en de) = Queue en (x:de)
   delete x (Queue en de) = Queue (L.delete x en) (L.delete x de)
   elemOf x (Queue en de) = x `elem` en && x `elem` de

instance Retrievable Queue where
   pop (Queue [x] de) = (x, Queue (reverse de) [])
   pop (Queue (x:en) de) = (x, Queue en de)
   peek (Queue [] _) = Nothing
   peek (Queue (x:_) _) = Just x

instance Mo.Monoid (Queue a) where
   mempty = new
   mappend (Queue en1 de1) (Queue en2 de2) =
      let en' = en1 ++ reverse de1 ++ en2 ++ reverse de2
          de' = []
      in
         Queue en' de'
