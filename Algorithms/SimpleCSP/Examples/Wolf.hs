{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

-- |CSP example for "bring items to the other shore"-type of problems:
--  Items i1,...,in are on one side of the shore and there's a boat
--  with limited passenger size. The aim is to transport all items
--  to the other side without leaving certain conflicting items
--  (e.g. a wolf and a sheep) unattended (e.g without a farmer).
module Algorithms.SimpleCSP.Examples.Wolf where

import Algorithms.SimpleCSP
import Prelude.Unicode
import Control.Monad
import Data.List
import Data.List.Unicode

-- |Position of a boat.
data Boat = West | East deriving (Eq, Show, Read, Enum, Ord)
-- |Objects on the shore.
data Object = Wolf
              | Goat
              | Cabbage
              | Fire
              | Stick
              | Farmer deriving (Eq, Show, Read, Enum, Ord)

type Objects = ([Object], [Object])
type GameState = (Objects, Boat)

-- |Nondeterminisitically moves between 1 and n items via boat to the other side,
--  always, taking the Farmer.
nextMove :: GameMove Int GameState
nextMove boatSize x =
   case x of
        ((w,e),West) → liftM (,East) (transport w e)
        ((w,e),East) → liftM (\(e',w') → ((w',e'),West)) (transport e w)
   where transport from to = do passengers ← chooseBetween 1 boatSize from
                                let from' = from \\ (Farmer:passengers)
                                    to' = nub $ Farmer:passengers ++ to
                                return (from', to')

-- |Returns true iff all items have arrived on the eastern side.
goalState :: Int → GameState → Bool
goalState _ (([],_),East) = True
goalState _ _             = False

-- |Solves the simple problem of a 2-passenger boat and
--  a farmer, a wolf, a goat, and a cabbage.
problem1 :: [Plan GameState]
problem1 = doPlan nextMove goalState constraints1 boatSize1 begin1

-- |Solves the more complex version: a 3-passenger boat,
--  and a farmer, a wolf, a goat, a cabbage, a fire and a stick.
problem2 :: [Plan GameState]
problem2 = doPlan nextMove goalState constraints2 boatSize2 begin2

boatSize1 :: Int
boatSize1 = 1
boatSize2 :: Int
boatSize2 = 2

begin1 :: GameState
begin1 = (([Wolf, Goat, Cabbage, Farmer], []), West)

begin2 :: GameState
begin2 = (([Wolf, Goat, Cabbage, Stick, Fire, Farmer], []), West)

wolfGoatNotAlone :: Constraint Int GameState
wolfGoatNotAlone _ ((w,e),_) = c' w ∧ c' e
   where c' set | Wolf ∈ set ∧ Goat ∈ set ∧ Farmer ∉ set = False
                | otherwise                              = True

cabbageGoatNotAlone :: Constraint Int GameState
cabbageGoatNotAlone _ ((w,e),_) = c' w ∧ c' e
   where c' set | Goat ∈ set ∧ Cabbage ∈ set ∧ Farmer ∉ set = False
                | otherwise                                 = True

fireStickNotAlone :: Constraint Int GameState
fireStickNotAlone _ ((w,e),_) = c' w ∧ c' e
   where c' set | Fire ∈ set ∧ Stick ∈ set ∧ Farmer ∉ set = False
                | otherwise                               = True

wolfStickNotAlone :: Constraint Int GameState
wolfStickNotAlone _ ((w,e),_) = c' w ∧ c' e
   where c' set | Wolf ∈ set ∧ Stick ∈ set ∧ Farmer ∉ set = False
                | otherwise                               = True

farmerWithBoat :: Constraint Int GameState
farmerWithBoat _ ((w,_),West) = Farmer ∈ w
farmerWithBoat _ ((_,e),East) = Farmer ∈ e

constraints1 :: Constraint Int GameState
constraints1 = constraints [wolfGoatNotAlone,
                            cabbageGoatNotAlone,
                            farmerWithBoat]

constraints2 :: Constraint Int GameState
constraints2 = constraints [wolfGoatNotAlone,
                            cabbageGoatNotAlone,
                            fireStickNotAlone,
                            wolfStickNotAlone,
                            farmerWithBoat]

-- |Prints a plan to the console.
printPlan :: Plan GameState → IO ()
printPlan p = void $ foldM1 printStep $ reverse p
   where printStep :: GameState -> GameState -> IO GameState
         printStep (prev, b1) (cur,b2) = do

            let dir = case (b1,b2) of (East,West) → putStr " <== "
                                      _           → putStr " ==> "
                w = putStr $ show $ fst prev
                e = putStr $ show $ snd prev
                movement = putStr $ show $ fst prev ∆ fst cur
            sequence_ [w,dir,movement,dir,e, putStrLn ""]
            return (cur,b2)