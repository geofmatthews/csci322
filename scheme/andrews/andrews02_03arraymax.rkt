#lang racket
(require "andrews.rkt")

(define n 500)
(define (w) (sleep 0.001))

(define a (makev n))
(for i from 0 to n (vset! a i 
                          (random 100000)
                          ))

(define m 0)

(define (max01)
  (set! m 0)
  (for i from 0 to (- n 1)
    (w)
    (when (> (vref a i) m)
      (w)
      (set! m (vref a i))
      ;(w)
      )))

(define (max02)
  (set! m 0)
  (co for i from 0 to (- n 1)
      (w)
      (when (> (vref a i) m)
        (w)
        (set! m (vref a i))
        ;(w)
        )))

(define (max03)
  (set! m 0)
  (co for i from 0 to (- n 1)
      (w)
      (atomic
       (w)
       (when (> (vref a i) m)
         (w)
         (set! m (vref a i))
         ;(w)
         ))))

(define (max04)
  (set! m 0)
  (co for i from 0 to (- n 1)
      (w)
      (when (> (vref a i) m)
        (w)
        (atomic
         (w)
         (when (> (vref a i) m)
           (w)
           (set! m (vref a i))
          ; (w)
           )))))

(for-each (lambda (max) (time (begin (max) (displayln m))))
          (list max01 max02 max03 max04))
