;; +--------------------------------------------------------------------------+
;; | How many ways are there to pebble a full binary tree of depth D, using N |
;; | pebbles?                                                                 |
;; +--------------------------------------------------------------------------+

(load "syntax.scm")

;; all-configs : (Nat, Nat) -> [Tree]
;; `all-pebbleable` enumerates all (labeled) full binary trees of a given
;; depth that are pebbleable (see below).
(define (all-pebbleable depth amt)
  (filter pebbleable? (perms depth amt)))

;; perms : (Nat, Nat) -> [Tree]
;; `perms` generates all full binary trees of a certain depth with a given
;; number of pebbles placed on their vertices.
(define (perms depth amt)
  (if (zero? depth)
    (return (Leaf amt))
    (list-comp (Inner top left right)
      (<- top     (range 0 amt))
      (<- in-left (range 0 (- amt top)))
      (<- left    (perms (- depth 1) in-left))
      (<- right   (perms (- depth 1) (- amt top in-left))))))

;; pebbleable? : (Tree Nat) -> Bool
;; `pebbleable?` determines whether its argument is pebbleable, in a pebbling
;; domination sense.
(define (pebbleable? tree)
  (with-tree tree
    (Leaf (v) (> v 0))
    (Inner (t l r)
      (let ((to-l-top (muster l))
            (to-r-top (muster r)))
        (let ((over-l (/2 to-l-top))
              (over-r (/2 to-r-top)))
          (and (> (+ over-l over-r t) 0)
               (pebbleable? (add-to-root l (/2 (+ t over-r))))
               (pebbleable? (add-to-root r (/2 (+ t over-l))))))))))

;; muster : (Tree Nat) -> Nat
;; `muster` determines how many pebbles can be transported to the root of a
;; tree.
(define (muster tree)
  (with-tree tree
    (Leaf (v) v)
    (Inner (t l r)
      (+ t
         (/2 (muster l))
         (/2 (muster r))))))

;; add-to-root : (Tree Nat) -> Tree Nat
;; `add-to-root` creates a new tree by adding a certain number of pebbles to
;; the root of an existing tree.
(define (add-to-root tree amt)
  (with-tree tree
    (Leaf (v) (Leaf (+ v amt)))
    (Inner (t l r)
      (Inner (+ t amt) l r))))

;; Leaf : (a) -> Tree a
;; `Leaf` constructs a leaf.
(define (Leaf n) n)

;; Inner : (a, Tree a, Tree a) -> Tree a
;; `Inner` constructs an interior vertex.
(define (Inner top left right) (list top left right))

(define leaf? number?)
(define (leaf-value l) l)

(define tree-top car)
(define tree-left cadr)
(define tree-right caddr)

;; range : (Int, Int) -> [Int]
;; `range` creates a list of integers, from `low` to `high`, including the
;; upper bound.
(define (range low high)
  (if (> low high)
    '()
    (cons low
          (range (+ low 1) high))))

;; /2 : (Int) -> Int
;; `/2` performs integer division by 2 (throwing away the remainder).
(define (/2 n) (quotient n 2))

;; >>= : ([a], (a -> [b])) -> [b]
;; `>>=` is the monadic "bind"/"chain" operation for lists.
(define (>>= xs f)
  (apply append (map f xs)))

;; return : a -> [a]
;; `return` is the monadic "return"/"pure" operation for lists.
(define (return n) (list n))
