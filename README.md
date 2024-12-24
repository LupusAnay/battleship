# Haskell interview

## Problem statement

The programming task is to create a Haskell program that will play Battleship against another
program. In order to do this the implementation will receive a function parameter that your
implementation can call to evaluate the results of a shot:

```Haskell
data Board

data ShootResult = Hit | Miss | HitSunk

type ShootFn = Int -> Int -> State Board ShootResult

sinkAllShips :: Int -> Int -> List Int -> ShootFn -> IO ()
sinkAllShips n m shipLengths shoot =
  ...
```

The program should print the shots and the results to the console. The game rules are as follows:
- only the program can shoot
- the ships are arranged in an `n * m` rectangle area
- the ship lengths are provided as an input
- the arrangement of the ships are kept secret from the program
- ships cannot touch each other horizontally, vertically or diagonally

The goal is to create the implementation of the `sinkAllShips` function.
Advice:
- focus on creating a program that works first, optimizations for runtime and/or number of shots are
  secondary
- you can make modifications to the provided code if you would like
- partial solutions are also accepted and will be evaluated
