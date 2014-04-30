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

data VariableStratum v = VS [v]
data ConstraintStratum c = CS [c]

type Stratum v c = (VariableStratum v, ConstraintStratum c)

