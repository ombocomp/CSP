-- |Simple constraint graph which model directed, binary constraints
--  and variables with finite, discrete domains.
module ConstraintGraph (
   Constraint(..),
   ConstraintGraph(..),
   mkCG,
   outConstraints,
   possibleValues,
   updateConstraints,
   violatedConstraintsPresent,
   mostConstrainedVariable,
   ) where 

import Prelude hiding (pred)
import Control.Arrow (second, (&&&))
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Foldable as F
import Data.Foldable (Foldable)
import Data.Eq
import Data.Maybe (fromJust)

-- |A directed, binary constraint.
data Constraint v a = Constraint{from::v,            -- ^The source variable.
                                 to::v,              -- ^The target variable.
                                 tryAssign::a -> [a] -- ^The remaining possible values
                                                     --  for the target if the source
                                                     --  is assigned a certain value.
                                 }


-- |A variable-indexed collection of constraints.
--  The keys are the variables, the values are tuples consisting
--  of the variables' domains and their outgoing constraints.
data ConstraintGraph v a = CG{cgvars::Map.Map v ([a], [Constraint v a]),
                              cgdoms::Map.Map [a] [v]}


-- |Creates a constraint graph out of a collection of
--  constraints and a list of variable-domain pairs.
mkCG :: (Foldable d, Ord v, Ord a)
     => d (Constraint v a)  -- ^The collection of constraints.
     -> [(v,[a])]           -- ^List of variables with their initial domains.
     -> ConstraintGraph v a -- ^Resulting constraint graph.
mkCG constraints variables = CG{cgvars=vars,cgdoms=doms}
   where 
         -- The CG with just the variables and domains.
         cgSkeleton = Map.fromList $ map (\(v,d) -> (v,(d,[]))) variables

         -- fold over the constraints by inserting them into the CG
         vars = F.foldl' insert' cgSkeleton constraints

         insert' m c = Map.insertWith addConstraint (from c) ([],[c]) m
         addConstraint (_,[c]) (d,cs) = (d,c:cs)


         doms = Map.fromList $ groupByKey (==) $ map switch variables

-- |Gets the list of constraints going out from a variable v.
outConstraints :: (Ord v) => v -> ConstraintGraph v a -> [Constraint v a]
outConstraints v = snd . fromJust . Map.lookup v . cgvars

-- |Gets the currently possible values for a variable.
possibleValues :: Ord v => v -> ConstraintGraph v a -> [a]
possibleValues v = fst . fromJust . Map.lookup v . cgvars

-- |Updates the constraint with a variable assignment.
updateConstraints :: (Eq v, Ord v, Eq a, Ord a) => v -> a -> ConstraintGraph v a -> ConstraintGraph v a
updateConstraints v a m = m{cgvars=vars',cgdoms=doms'}
   where
      newDoms = intersect                       -- Merge them (in case multiple constraints had the same target)
                $ ((v,[a]):)                    -- Add the current assignment
                $ map (to &&& flip tryAssign a) -- Get the constrained values of v's targets.
                $ outConstraints v m            -- Get the outgoing constraints of v


      -- Creates a map which only contains update data
      -- for the domains, not the constraints.
      varUpdates = Map.fromList
                   $ map (\(v',d) -> (v',(d,[])))
                   newDoms

      vars' = Map.unionWith f (cgvars m) varUpdates
         where f (_,c) (d,_) = (d,c)

      -- Creates a map which contains update data for the
      -- domains/variables (here, the domains are the keys
      -- and the variables are the values, which get assigned
      -- to different keys due to domain changes.
      domUpdates = Map.fromList
                   $ map extractFst                -- Switch the components and group them by the domains.
                   $ L.groupBy (equating fst)
                   $ map switch
                   newDoms
      -- TODO: possible change to L.intersect to const,
      -- since we assume that the values in domUpdates are
      -- exactly the permissible ones.
      doms' = Map.unionWith L.intersect (cgdoms m) domUpdates

-- |Given a list of variable-domain pairs,
--  calculates the intersection of all occurring domains for
--  each variable.
intersect :: (Eq v, Eq a) => [(v,[a])] -> [(v,[a])]
intersect =  map (second intersectDomains) . groupByKey (==)
   where 
      intersectDomains = foldr1 L.intersect 


-- |Returns True iff there are variables with no possible values.
violatedConstraintsPresent :: (Ord v,Ord a) => ConstraintGraph v a -> Bool
violatedConstraintsPresent m = toBool (do emptyVars <- Map.lookup [] $ cgdoms m
                                          if null emptyVars then Nothing else Just True)
   where toBool Nothing = False
         toBool (Just x) = x

-- |Gets the most constrained, unassigned variable, i.e.
--  the one with the smallest number of possible values.
mostConstrainedVariable :: ConstraintGraph v a -> Maybe a
mostConstrainedVariable = undefined

-------- Helper functions

-- |Given a list of pairs [(X,y1),(X,y2),...,(X,yn)],
--  returns (X,[y1,...,yn]).
extractFst :: [(a,b)] -> (a,[b])
extractFst ((k,v):xs) = (k, v : map snd xs)

-- |Groups a list of key-value pairs by key.
groupByKey :: (k -> k -> Bool) -- ^Predicate 'pred' determining the equality of two keys.
           -> [(k,v)] -- ^List of key-value pairs.
           -> [(k,[v])] -- ^The given list, grouped by keys according to 'pred'.
groupByKey pred = map extractFst . L.groupBy (\(k1,_) (k2,_) -> pred k1 k2) 

-- |Switches the components of a tuple.
switch :: (a,b) -> (b,a)
switch (x,y) = (y,x)
