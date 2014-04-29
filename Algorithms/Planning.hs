{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

-- |Simple CSP which naively generates all possible plans for a problem.
--  This algorithm is sufficient for small probl, but quickly
--  becomes infeasible to execute for larger instances.
module Algorithms.Planning where

import Prelude.Unicode
import Control.Monad
import Control.Arrow
import Data.List

type Constraint params state = params → state → Bool

type Plan state = [state]
type PreviousStates state = [state]
type PlanningStep state = (PreviousStates state,[Plan state])

type GameMove params state = params → state → [state]
type GoalState params state = params → state → Bool

-- Helper functions

-- |Powerset of a set.
powerset :: [a] → [[a]]
powerset = filterM (const [False,True])
-- |Returns all subsets 's' of a set such that. 'n <= |s| <= m'.
chooseBetween :: Int -- ^The lower bound n.
              → Int -- ^The upper bound m.
              → [a] -- ^The set whose subsets are to be returned.
              → [[a]]
chooseBetween n m = filter (\p -> length p >= n ∧ length p <= m) ∘ powerset

-- |Monad analogue to @foldl1@.
foldM1 :: Monad m ⇒ (a → a → m a) → [a] → m a
foldM1 f = uncurry (foldM f) ∘ (head &&& tail)

-- |@unfoldr@ with concatenation of the results.
boundedIterate :: (b → Maybe ([a],b)) → b → [a]
boundedIterate f s = concat $ unfoldr f s


-- |Nondeterministically makes one valid, non-redundant step in the plan.
--  If no steps are possible, Nothing is returned.
makeStep :: (Eq state)
         ⇒ GameMove params state -- ^The move-generating functions.
         →  Constraint params state -- ^ Active constraints.
         →  params -- ^ Fixed parameters of the game.
         →  PlanningStep state -- ^Hitherto generated steps.
         →  Maybe ([Plan state], PlanningStep state)
makeStep nextMove constraintsFulfilled params (previousStates,plans) =
   let newplans = do (laststep:oldsteps) ← plans
                     next ← nextMove params laststep
                     guard $ constraintsFulfilled params next
                     guard $ next ∉ previousStates
                     return $ next:laststep:oldsteps
       res = (previousStates ++ map head newplans, newplans)
   in if null newplans then Nothing else Just (newplans, res)

-- |Merges multiple constraints into one.
constraints :: [Constraint params state] → Constraint params state
constraints xs p obj = all (\x → x p obj) xs

-- |Solves a problem and returns the list of possible plans.
doPlan :: (Eq state)
       ⇒ GameMove params state
       →  GoalState params state
       →  Constraint params state
       →  params
       →  state
       → [Plan state]
doPlan gameMove goalState c params begin = solutions
   where makeStep' = makeStep gameMove c params
         solutions = do
            plan <- boundedIterate makeStep' ([begin], [[begin]])
            guard $ (goalState params ∘ head) plan
            return plan





