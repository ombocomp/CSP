module CSP where

import Prelude hiding (init, succ, pred)
import Control.Arrow
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Foldable (Foldable)
import Data.Ord (comparing)
import Data.Maybe
import Control.Monad (guard)
import DataStructure
import Search
import ConstraintGraph


data VariableStratum v = VS [v]
data ConstraintStratum c = CS [c]

type Stratum v c = (VariableStratum v, ConstraintStratum c)
type Solution d v a = d (v,a)

data CSPNode v a = CSPNode{cspnodeAssignments::Map.Map v a,
                           cspnodeRemaining::[v],
                           cspnodeCG::ConstraintGraph v a}

instance (Eq v, Ord a) => Eq (CSPNode v a) where
   (==) = equating cspnodeAssignments

instance (Ord v, Ord a) => Ord (CSPNode v a) where
   compare = comparing cspnodeAssignments

csp :: (Retrievable d1, Foldable d1, Ord v, Ord a)
    => d1 (Constraint v a)
    -> [(v,[a])]
    -> Set (CSPNode v a)
csp constraints variables = mkSolution $ dfs init succ goal
   where -- initial node: no assignments, all variables remaining,
         --  fresh constraint graph
         init = CSPNode Map.empty
                        (map fst variables)
                        (mkCG constraints variables)

         -- successor: take the next variable and assign it every possible value.
         succ (CSPNode _ [] _) = S.empty
         succ (CSPNode a (n:ns) cg) = 
            S.fromList (do d <- fst $ fromJust $ Map.lookup n (cgvars cg)
                           let cg' = updateConstraints n d cg
                           guard $ not $ violatedConstraintsPresent cg'
                           return $! CSPNode (Map.insert n d a) ns cg')


         -- a goal node has been reached if no variables remain to be assigned.
         goal (CSPNode _ [] _) = True
         goal _ = False

         mkSolution = undefined

mkCG :: (Foldable d, Ord v, Ord a)
     => d (Constraint v a)
     -> [(v,[a])]
     -> ConstraintGraph v a
mkCG constraints variables = CG{cgvars=vars,cgdoms=doms}
   where 
         -- The CG with just the variables and domains.
         cgSkeleton = Map.fromList $ map (\(v,d) -> (v,(d,[]))) variables

         -- fold over the constraints by inserting them into the CG
         vars = F.foldl' insert' cgSkeleton constraints

         insert' m c = Map.insertWith addConstraint (from c) ([],[c]) m
         addConstraint (_,[c]) (d,cs) = (d,c:cs)


         doms = Map.fromList $ groupByKey (==) $ map switch variables
