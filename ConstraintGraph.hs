-- |Simple constraint graph which model directed, binary constraints
--  and variables with finite, discrete domains.
module ConstraintGraph where 

import Control.Arrow (second, (&&&))
import qualified Data.List as L
import qualified Data.Map as Map
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

-- |Gets the list of constraints going out from a variable v.
outConstraints :: (Ord v) => v -> ConstraintGraph v a -> [Constraint v a]
outConstraints v = snd . fromJust . Map.lookup v . cgvars

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
intersect =  map (second intersectDomains . extractFst) . L.groupBy (equating fst)
   where 
      intersectDomains = foldr1 L.intersect 

-- |Given a list of pairs [(X,y1),(X,y2),...,(X,yn)],
--  returns (X,[y1,...,yn]).
extractFst :: [(a,b)] -> (a,[b])
extractFst ((k,v):xs) = (k, v : map snd xs)

groupByKey :: (k -> k -> Bool) -> [(k,v)] -> [(k,[v])]
groupByKey pred = map extractFst . L.groupBy (\(k1,_) (k2,_) -> pred k1 k2) 

-- |@Eq@-analogue of @Data.Ord.comparing@.
equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f x y = f x == f y

-- |Switches the components of a tuple.
switch :: (a,b) -> (b,a)
switch (x,y) = (y,x)

-- |Returns True iff there are variables with no possible values.
violatedConstraintsPresent :: (Ord v,Ord a) => ConstraintGraph v a -> Bool
violatedConstraintsPresent m = toBool (do emptyVars <- Map.lookup [] $ cgdoms m
                                          if null emptyVars then Nothing else Just True)
   where toBool Nothing = False
         toBool (Just x) = x