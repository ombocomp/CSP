-- |Simple CSP solver which returns the set of all solutions for a
--  CSP. It performs both forward checking (for arc consistency) and
--  utilizes the ``most constrained variable'' variable selection
--  heuristic.
module CSP (
   Solution,
   csp
   )where

import Prelude hiding (init, succ, Functor(..))
import Data.Functor.MultiParam
import Data.Set (Set)
import Data.Eq
import Data.Maybe (fromJust, listToMaybe)
import Data.List (sortBy, (\\))
import qualified Data.Set as S
import qualified Data.Map as Map
import Data.Foldable (Foldable)
import Data.Ord (comparing)
import Control.Monad (guard)
import DataStructure
import Search
import ConstraintGraph


-- |A solution to a CSP: a collection of variable-value pairs.
type Solution d v a = d (v,a)

-- |A node in the CSP search tree.
data CSPNode v a = CSPNode{cspnodeAssignments::Map.Map v a, -- ^Past variable assignments.
                           cspnodeRemaining::[v], -- ^Remaining, unassigned variables.
                           cspnodeCG::ConstraintGraph v a -- ^Current constaint graph.
                           }

instance (Eq v, Ord a) => Eq (CSPNode v a) where
   (==) = equating cspnodeAssignments

instance (Ord v, Ord a) => Ord (CSPNode v a) where
   compare = comparing cspnodeAssignments

-- |Solves a CSP with finite-domain variables and returns
--  the set of all found solutions.
csp :: (Retrievable d1, Foldable d1, Retrievable d2, Ord v, Ord a, Ord (d2 (v,a)))
    => d1 (Constraint v a) -- ^A collection of constraints.
    -> [(v,[a])]           -- ^List of existing variables, with their initial domains.
    -> Set (Solution d2 v a)
csp constraints variables = fmap' mkSolution $ dfs init succ goal
   where -- initial node: no assignments, all variables remaining,
         --  fresh constraint graph
         init = CSPNode Map.empty
                        (map fst variables)
                        (mkCG constraints variables)

         -- successor: take the most constrained unassigned variables
         -- and assign it every possible value.
         succ (CSPNode _ [] _) = S.empty
         succ (CSPNode a ns cg) = 
            S.fromList (do let (nextVar,vals) = fromJust $ mostConstrainedVariable ns cg
                           val <- vals
                           let cg' = updateConstraints nextVar val cg
                           guard $ not $ violatedConstraintsPresent cg'
                           return $! CSPNode (Map.insert nextVar val a) (ns \\ [nextVar]) cg')


         -- a goal node has been reached if no variables remain to be assigned.
         goal (CSPNode _ [] _) = True
         goal _ = False

         -- extracts the assignments from a CSP, discarding the fluff.
         mkSolution = (`insertAll` new) . Map.toList . cspnodeAssignments

-- |Gets the most constrained variable from a list, i.e.
--  the one with the smallest number of possible values.
mostConstrainedVariable :: (Ord v, Ord a)
                        => [v] -- ^The list 'vs' of variables from which to select.
                        -> ConstraintGraph v a -- ^The constraint graph 'cg'.
                        -> Maybe (v,[a]) -- ^The element of 'vs' with the smallest
                                         --  current domain, according to 'cg'.
mostConstrainedVariable vs = listToMaybe
                             . filter ((`elem` vs) . fst)
                             . sortBy (comparing snd)
                             . variableDomains 