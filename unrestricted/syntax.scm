;; `list-comp` provides some new syntax for rudimentary list-comprehensions,
;; in the hope of cleaning expressions involving a lot of >>='s.
;; For example,
;;   (list-comp `(,x ,y) (<- x '(1 2 3)) (<- y '(a b)))
;;   => '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
(define-syntax list-comp
  (syntax-rules (<-)
    ((_ expr) expr)
    ((_ expr (<- x gen))
     (>>= gen (lambda (x) (return expr))))
    ((_ expr (<- x gen) (<- xs gens) ...)
     (>>= gen (lambda (x)
      (list-comp expr (<- xs gens) ...))))))

;; `with-tree` provides some additional syntax for working with binary trees.
;; You provide a tree, an expression to be evaluated in the case that it is a
;; leaf vertex, and an expression to be evaluated in the case that it is an
;; interior vertex. Additionally, the value of the leaf vertex, and of the
;; root vertex, left subtree, and right subtree, are bound to variables that
;; you supply before the respective expressions. For instance,
;;   (with-tree (Inner 3 (Leaf 0) (Leaf 1))
;;     (Leaf (v)      `(a leaf whose value is ,v))
;;     (Inner (t l r) `(an inner vertex whose left-subtree is ,l)))
;;   => (an inner vertex whose left subtree is 0)
(define-syntax with-tree
  (syntax-rules (Leaf Inner)
    ((with-tree tree
       (Leaf (v) leaf-expr)
       (Inner (t l r) inner-expr))
     (if (leaf? tree)
       ((lambda (v) leaf-expr) (leaf-value tree))
       ((lambda (t l r) inner-expr)
        (tree-top tree)
        (tree-left tree)
        (tree-right tree))))))
