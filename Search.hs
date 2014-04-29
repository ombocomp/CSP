-- |Generic tree and graph search algorithms.
module Search (
   NodePath(..),
   Merger,
   GoalChecker,
   SuccessorGenerator,
   search,
   bfs,
   dfs,
   gdfs) where

import DataStructure
import Prelude hiding (init, succ)
import Data.Set (Set)

data NodePath n = Node{node::n, nodePred::Set (NodePath n)}

type Merger n = [NodePath n] -> NodePath n
type GoalChecker n = n -> Bool
type SuccessorGenerator n = n -> Set n

-- |Generalized DFS with node duplicate elimination
--  and path merge operation.
gdfs :: (Eq node, Ord node)
    => node                    -- ^The root node of the search tree.
    -> SuccessorGenerator node -- ^Successor generator function.
    -> Merger node             -- ^Merging function for convering search paths.
    -> GoalChecker node        -- ^Goal checking function which terminates a search path.
    -> Set (NodePath node)     -- ^The set of found solutions, with their search paths.
gdfs = undefined


-- |Generic tree search without cycle checking.
--  Stops at any node for which the goal function is true and
--  returns the set of all found solutions.
search :: (Eq node, Ord node, Retrievable ds)
       => ds node -- ^The container for the nodes to be expanded.
                  --  A stack results in a DFS, a queue in a BFS.
       -> node    -- ^ The initial node in the search tree.
       -> SuccessorGenerator node -- ^The successor generator function for a node.
       -> GoalChecker node        -- ^The goal checking function for a node.
       -> Set node                -- ^Set of found solutions.
search cont init = search' (init `insert` cont)
   where search' nextNodes succ goal
            | isEmpty nextNodes = new
            | otherwise = let
                             (curNode, nextNodes') = pop nextNodes
                             nextNodes'' = insertAll (succ curNode) nextNodes'
                          in
                             if goal curNode then curNode `insert` search' nextNodes' succ goal
                             else search' nextNodes'' succ goal

-- |Depth-first tree search without cycle checking.
--  Stops at any node for which the goal function is true and
--  returns the set of all found solutions.
dfs :: (Eq node, Ord node)
    => node    -- ^ The initial node in the search tree.
    -> SuccessorGenerator node -- ^The successor generator function for a node.
    -> GoalChecker node        -- ^The goal checking function for a node.
    -> Set node                -- ^Set of found solutions.
dfs = search (new :: Stack node)

-- |Breadth-first tree search without cycle checking.
--  Stops at any node for which the goal function is true and
--  returns the set of all found solutions.
bfs :: (Eq node, Ord node)
    => node    -- ^ The initial node in the search tree.
    -> SuccessorGenerator node -- ^The successor generator function for a node.
    -> GoalChecker node        -- ^The goal checking function for a node.
    -> Set node                -- ^Set of found solutions.
bfs = search (new :: Queue node)
