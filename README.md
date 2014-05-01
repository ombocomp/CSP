CSP
===

A CSP solver for both static and dynamic CSPs in Haskell. Handles arbitrary binary constraints and performs forward checking (via arc consistency) and the "most constrained variable" selection heuristic. 

Static CSP
----------

The static CSP solver is found in CSP.hs. Static CSPs are specified via a list of variable-domain pairs [(v,[d])] and a collection of directed, binary constraints. A constraint from `v1 -> v2` can be any computable function `a -> [b]` which, given an assigment of a value `a` to `v1`, returns the list `[b]` of remaining possible values for `v2`.

Given a list of variables and a collection of constraints, the CSP solver returns the set of all solutions.

Dynamic CSP.
----------

Dynamic CSPs extend static ones by allowing the supply of a (potentially infinite) list of constraint collections `C` and a goal function `g`. Each entry in the list represents the active constraintd at time `t`. At each time `t`, the solver tries to solve the constraint graph at `C[t]`. Each found solution `S` is checked against `g` and, if `g(S) = True`, is returned. The solver then proceeds to `t+1` and tries extend the found solutions by trying to solve the constraint graph at `C[t+1]`. As long as the solver keeps finding solutions, it will contine and hence, termination is not guaranteed. The upside of this is that DCSPs can be used for unbounded planning.
