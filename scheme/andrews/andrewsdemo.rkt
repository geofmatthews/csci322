#lang racket
(require "andrews.rkt")

(define d displayln)

(define (demo01)
  (co (d 1) (d 2) (d 3) (d 4) (d 5))
  (displayln 'demo01done))
;(demo01)

(define (demo02)
  (co for i from 1 to 10 (d i))
  (displayln 'demo02done))
;(demo02)

(define (demo03)
  (co for i from 1 to 5 (atomic (d i) (d (* 2 i))))
  (displayln 'demo03done))
;(demo03)

(define (demo04)
  (define x 0)
  (co
   (await (> x 5) 
          (d "=====================> Yay!")
          (sleep 1)
          (d "x is bigger than 5!!!")
          )
   (let loop ()
     (when (< x 10)
       (atomic
        (set! x (+ x 1)))
       (sleep .0001)
       (atomic (d x))
       (sleep .001)
       (loop))))
  (displayln 'demo04done))
;(demo04)

(define n 5)
(define a (makev n))
(define b (makev n))
(define (demo05)
  (co for i from 1 to n 
      (vset! a i i)
      (vset! b i 0))
  (sleep 1)
  (for i from 1 to n
    (displayln (list i (vref a i) (vref b i))))
  (displayln 'demo05done))
;(demo05)