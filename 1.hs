{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (Left, Right)
import Data.List.Split (split)

data Direction = North | East | South | West
    deriving (Show, Enum)

-- enum doesn't wrap so...
next :: Direction -> Direction
next North = East
next East = South
next South = West
next West = North

prev :: Direction -> Direction
prev North = West
prev West = South
prev South = East
prev East = North


data Turn = Left | Right
    deriving (Show)

data Movement = Movement {turn :: Turn, distance :: Int}
    deriving (Show)

type Coord = (Int, Int)

data Location = Location {coord :: Coord, heading :: Direction}
    deriving (Show)

-- go from "R10" to (Movement Right 10)
parseMovement :: String -> Movement
parseMovement (x:xs)
    | x == 'L' = Movement Left amount
    | x == 'R' = Movement Right amount
    | otherwise = error "parse error"
    where amount = read xs::Int

-- apply a movement to a location
applyMovement :: Location -> Movement -> Location
applyMovement (Location x direction) (Movement Right amount) = move (Location x direction') amount
    where direction' = next direction
applyMovement (Location x direction) (Movement Left amount) = move (Location x direction') amount
    where direction' = prev direction

-- go forward by an amount
move :: Location -> Int -> Location
move (Location (x,y) North) amount = (Location (x,y+amount) North)
move (Location (x,y) East) amount = (Location (x+amount,y) East)
move (Location (x,y) South) amount = (Location (x,y-amount) South)
move (Location (x,y) West) amount = (Location (x-amount,y) West)

-- given a string like "R10, L3, R4" produce the list
-- [(Movement Right 10), (Movement Left 3), (Movement Right 4)]
createMovements :: String -> [Movement]
createMovements = map parseMovement . words

-- figure out where we end up after applying movements to the starting location
getFinalLocation :: Location
getFinalLocation = foldl applyMovement initialLocation (createMovements input)

-- where we begin
initialLocation :: Location
initialLocation = (Location (0,0) North)

-- input from the site (with commas stripped because forget doing that)
input :: String
input = "R3 L2 L2 R4 L1 R2 R3 R4 L2 R4 L2 L5 L1 R5 R2 R2 L1 R4 R1 L5 L3 R4 R3 R1 L1 L5 L4 L2 R5 L3 L4 R3 R1 L3 R1 L3 R3 L4 R2 R5 L190 R2 L3 R47 R4 L3 R78 L1 R3 R190 R4 L3 R4 R2 R5 R3 R4 R3 L1 L4 R3 L4 R1 L4 L5 R3 L3 L4 R1 R2 L4 L3 R3 R3 L2 L5 R1 L4 L1 R5 L5 R1 R5 L4 R2 L2 R1 L5 L4 R4 R4 R3 R2 R3 L1 R4 R5 L2 L5 L4 L1 R4 L4 R4 L4 R1 R5 L1 R1 L5 R5 R1 R1 L3 L1 R4 L1 L4 L4 L3 R1 R4 R1 R1 R2 L5 L2 R4 L1 R3 L5 L2 R5 L4 R5 L5 R3 R4 L3 L3 L2 R2 L5 L5 R3 R4 R3 R4 R3 R1"

-- figure out the taxicab distance from a location from the starting location
taxiCabDistance :: Location -> Int
taxiCabDistance (Location (x1, y1) _) = abs (y1 - y2) + abs (x1 - x2)
    where x2 = fst $ coord initialLocation
          y2 = snd $ coord initialLocation

solution :: Int
solution = taxiCabDistance getFinalLocation
