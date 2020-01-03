# Genetic algorithms in Haskell

This is just another one of those projects that I create to get me back into programming in a language. In this case, Haskell, and remembering how genetic algorithms work.


## Genetic programming

A work in progress of an implementation of genetic programming is contained in the `Tree` module.

This implements the `fitness`, `mutate`, and `crossover` methods of the `Genetic` typeclass.

Fitness is calculated by taking an list of target (input, output) pairs, evaluating the program for each of the inputs, and taking the sum of the differences between each program output and the target output. An additional factor is present to favor shorter programs (fewer nodes in the tree) over longer ones (more nodes in the tree).

Mutation is implemented as subtree mutation (Koza 1992), where a randomly chosen node (and its descendants) is replaced with a randomly generated subtree.

Crossover is implemented as subtree crossover.

## Todo

* Selection methods, that work for all population types.
* Different tree mutation operators
* Grow trees differently