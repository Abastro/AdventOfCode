# Advent Of Code Solutions (Haskell)
- With a flavor of code golfing
- More oriented towards short length & running speed than readability

## Structure
- 1 module per 1 problem
- Source is located inside `/src/Y{year}/Prob{number}.hs`
- Input needs to go into `/input/{year}/{number}` for corresponding problem

## How to run
- `cabal run` and put Year and Code
- Code: e.g. `5.Fst` for the first problem, `5.Snd` for the second problem
- Code `all` to run through problems 01-25

## Benchmark
- Done on *Intel(R) Core(TM) i7-6500U CPU @ 2.50GHz* with 4GB+4GB DDR3 RAM
  - Multithreading not used
  - Input reading time included
- [AoC2021 Benchmark](https://htmlpreview.github.io/?https://github.com/Abastro/AdventOfCode/blob/master/aoc2021bench.html)
