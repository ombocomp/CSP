module DCSP where

data VariableStratum v = VS [v]
data ConstraintStratum c = CS [c]

type Stratum v c = (VariableStratum v, ConstraintStratum c)

