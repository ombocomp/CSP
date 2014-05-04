-- |Dynamic CSP solver which returns the set of all solutions
--  to a dynamic CSP.
--  The solver takes a (potentially infinite) list of
--  constraint graphs, each of them representing the active
--  constraints in a single time step.
--  
--  Since infinite lists of constraint graphs are permissible,
--  dynamic CSPs can be used for open-ended planning,
--  but do not guarantee termination.
module DCSP where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Eq
import Data.List (groupBy)
import Data.Foldable (Foldable)
import Data.Ord (comparing)
import DataStructure
import ConstraintGraph

data VariableStratum v = VS [v]
data ConstraintStratum c = CS [c]

type Stratum v c = (VariableStratum v, ConstraintStratum c)

-- |A solution to a CSP: a collection of variable-value pairs.
type Solution d v a = d (v,a)

-- |A DAG structure, consisting of a node and the set of its
--  predecessors (i.e. its incoming neighbours).
data DAG n = DAG{dagNode::n,dagPred::Set (DAG n)} deriving (Eq, Ord)

-- |Quick show instance for DAG.
instance Show n => Show (DAG n) where
   show (DAG n ns) | Set.null ns = show n
                   | otherwise = show n ++ " <-" ++ show (Set.toList ns)

-- tests for mergeDAG

--d1 = DAG 1 (Set.fromList [])
--d2 = DAG 2 (Set.fromList [d1])
--d3 = DAG 3 (Set.fromList [d1])
--d4 = DAG 4 (Set.fromList [d2,d3])
--d5 = DAG 5 (Set.fromList [d1])
--d4' = DAG 4 (Set.fromList [d5])

-- |Merges a set of DAGs if they have parts in common.
--  Formally, if '{(DAG x p1), (DAG x p2),...,(DAG x pN)}' is a
--  subset of the input set, then, then that subset will be replaced with
--  '{(DAG x mergeDAG {p1 `union` ... `union` pn})}'.
mergeDAG :: Ord n => Set (DAG n) -> Set (DAG n)
mergeDAG xs | Set.null xs || Set.size xs == 1 = xs
            | otherwise = Set.fromList $ map merge xs'
         -- Group the input by node (=determine the mergeable subsets).
   where xs' = groupBy (equating dagNode) $ Set.toList xs
         -- The one-step merge operation which extracts 'x'.
         -- from the set '{(DAG x p1), (DAG x p2),...,(DAG x pN)}'.
         -- The case for the empty list is undefined, since 'groupBy'
         -- doesn't create empty groups.
         merge (DAG y ys:ys') = DAG y (Set.unions $ ys:map dagPred ys')


-- |A node in a dynamic CSP search DAG.
data DCSPNode v a = DCSPNode{dcspNodeAssignments :: DAG (Map.Map (v,Int) a),
                             dcspNodeCG :: ConstraintGraph (v,Int) a}

instance (Eq v, Ord a) => Eq (DCSPNode v a) where
   (==) = equating dcspNodeAssignments

instance (Ord v, Ord a) => Ord (DCSPNode v a) where
   compare = comparing dcspNodeAssignments

-- |
dcsp :: [d (Constraint (v,Int) a)]
     -> [(v,[a])]
     -> Set (Solution d2 (v,Int) a)
dcsp constraints variables = undefined