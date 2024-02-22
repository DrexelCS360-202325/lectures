#lang racket

(define zero (lambda (f) (lambda (z) z)))
(define one  (lambda (f) (lambda (z) (f z))))
(define two  (lambda (f) (lambda (z) (f (f z)))))

(define (church->nat n)
  ((n add1) 0))

(define (nat->church n)
  (lambda (f)
    (lambda (z)
      (if (= n 0)
          z
          (f (((nat->church (- n 1)) f) z))))))

(define succ
  'undefned)

(define plus
  'undefined)

(define mult
  'undefined)

