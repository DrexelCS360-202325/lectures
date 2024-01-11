#lang racket
(require "tree.rkt")

(provide (all-defined-out))

;;
;; Folds
;;

; Right fold

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (first xs) (foldr f z (rest xs)))))

(show-tree (foldr (show-function 'cons) 'null '(1 2 3 4)))

; Left fold

(define (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (first xs)) (rest xs))))

(show-tree (foldl (show-function 'cons) 'null '(1 2 3 4)))
