CSP
===

A CSP solver for both static and dynamic CSPs in Haskell. Handles arbitrary binary constraints and performs forward checking (via arc consistency) and the "most constrained variable" selection heuristic. 

Static CSP
----------

The static CSP solver solves the classical CSP problem: given a list of variables (with the domains) and a constraint graph, find a variable assignment s.t. the constraint graph is satisfied.

The solver is found in the CSP module and makes use of the following two heuristics:

1. Forward checking for arc consistency - assignments which would *immediately* lead to a variable having no more possible values are not made.
2. Most constrained variables first - the variable which has the least number of possible values is assigned before all other variables, minimizing the branching factor of the search tree.

The solver then delivers the set of all satisfying variable assignments.

#### Formal definition

Input:
* A list of variable-domain pairs `[(v,[d])]`.
* A collection of constraints `d (c v d)`, where `d` is a collection and `c v d` is a constraint with variable type `v` and domain type `d`. Constraints go from a variable `v1` to another variable `v2` and contain a function `d -> [d]`, which, given a hypothetical assignment `d` to `v1`, returns the list `[d]` of remaining possible values for `v2`.

Output:
* A set of solutions `Set Solution`, where `Solution = [(v,d)]`, representing the variables with their assigned values.

Dynamic CSP.
----------

Dynamic CSPs extend static ones by allowing the supply of a (potentially infinite) list of constraint collections `C` and a goal function `g`. Each entry in the list represents the active constraintd at time `t`. At each time `t`, the solver tries to solve the constraint graph at `C[t]`. Each found solution `S` is checked against `g` and, if `g(S) = True`, is returned. The solver then proceeds to `t+1` and tries extend the found solutions by trying to solve the constraint graph at `C[t+1]`. As long as the solver keeps finding solutions, it will contine and hence, termination is not guaranteed. The upside of this is that DCSPs can be used for unbounded planning.
