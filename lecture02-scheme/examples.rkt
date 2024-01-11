#lang racket
(require "tree.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; List -> Integer
;
; Compute the length of the list l
;   The length of the empty list is 0.
;   The length of a non-empty list is 1 + the length of the cdr of the list.
;
; Examples:
;   (length '()) => 0
;   (length '(1 2)) => 2
;   (length '(1 (2 3))) => 2
;

(define (length xs)
  'not-implemented)

; List-of-numbers -> Number
;
; Compute the sum of a list of integers
;
; Example:
;   (sum '(1 2 3 4)) => 10

(define (sum xs)
  'not-implemented)

; List Integer -> Any
;
; Return the nth element of a list, counting from 0.
;
; Examples:
;  (nth 0 '(1 2 3)) => 1
;  (nth 2 '(1 2 3)) => 3lecture02-scheme

(define (nth n xs)
  'not-implemented)

; List List -> List
;
; This function is called append in the Scheme standard library.
;
; Think carefully about the base case and about which argument to recurse on!
;
; Concatenate the lists l1 and l2 (append l2 to l1)
;   The concatention of l1 and l2 is equal to l2 if l1 is null.
;   Otherwise it is the list whose first element (car) is the first
;   element of l1 and whose tail (cdr) is equal to the concatention
;   of the tail of l1 and l2.
;
; Example:
;   (concat '(1 2 3) '(4 5 6)) => (1 2 3 4 5 6)
;

(define (concat xs ys)
  'not-implemented)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Higher Order Function Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You may use the built-in functions map, filter, and fold

; List-of-numbers -> List-of-numbers
;
; Calculate the squares of a list of integers. Make the function non-recursive.
;
; Example:
;   (squares '(1 2 3 4 5)) => '(1 4 9 16 25)
;

(define (squares xs)
  'not-implemented)

; List-of-numbers -> Number
;
; Calculate the sum of squares of a list of integers. Make the function non-recursive.
;
; Example:
;   (sum-of-squares '(1 2 3 4 5)) => 55
;

(define (sum-of-squares xs)
  'not-implemented)

;
; List-of-integers -> List-of-integers
;
; Write a non-recursive function that takes a list and returns all the even
; integers in the list
;
; Examples:
;   (only-even '(1 2 3 4 5)) => '(2 4)

(define (only-even xs)
  'not-implemented)
