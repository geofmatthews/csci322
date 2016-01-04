#lang racket

;; Define functions with lambda:

(define f (lambda (n) (+ n n)))
(f 5)

;; Define functions without lambda:

(define (g n) (+ n n))
(g 5)