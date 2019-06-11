# pebbling
Resources for T. Lewis' pebbling domination research.

This repository contains a number of programs created to test various theories regarding the pebbling domination problem.

## unrestricted/full-binary.scm

This program allows one to generate all (labeled) full-binary trees of a given depth that are pebbleable using a given amount of pebbles.
For instance, to enumerate all trees of depth 3 that are pebbleable using 7 pebbles, we evaluate:

```scheme
(all-pebbleable 3 7)
```

This produces a list of trees represented as S-Expressions.
For instance:

```scheme
(all-pebbleable 2 4)

;; => ((0 (2 0 0) (2 0 0))
;;     (1 (1 0 0) (2 0 0))
;;     (1 (2 0 0) (1 0 0))
;;     (2 (1 0 0) (1 0 0))
;;     (4 (0 0 0) (0 0 0)))
```

## symmetric/Symmetric.hs
This program generates all *symmetric* binary trees that are pebbleable using a certain amount of material.
As such, it runs much faster that the program above.
The function `allValid` can be used to generate representations of these trees as follows:

```haskell
allValid 2 4
-- => [[0,2,0],[2,1,0],[4,0,0]]
```

Each list represents a symmetric binary tree where the leftmost element is the amount of material located at the root of the tree, the next element is the amount located at each of the root's children, etc.

The program can also be used as an executable as follows (suppose that it has been compiled to a binary called `pebsym`):

```
wjl$ ./pebsym 2 4
Results (root marked by "r"):
 r
[0,2,0]
[2,1,0]
[4,0,0]
```