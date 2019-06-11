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
