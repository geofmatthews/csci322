#lang racket
(require "andrews.rkt")

(define n 100)

(define a (makev n))
(for i from 0 to n (vset! a i 
                          (random 1000)
                          ))

(define m 0)

(define (max01)
  (set! m 0)
  (for i from 0 to (- n 1)
    (when (> (vref a i) m)
      (sleep .1)
      (set! m (vref a i)))))

(when #t
  (max01)
  (displayln m))

(define (max02)
  (set! m 0)
  (co for i from 0 to (- n 1)
      (when (> (vref a i) m)
        (sleep .1)
        (set! m (vref a i)))))

(when #f
  (max02)
  (displayln m))

(define (max03)
  (set! m 0)
  (co for i from 0 to (- n 1)
      (atomic
       (when (> (vref a i) m)
         (sleep .1)
         (set! m (vref a i))))))

(when #f
  (max03)
  (displayln m))

(define (max04)
  (set! m 0)
  (co for i from 0 to (- n 1)
      (when (> (vref a i) m)
        (sleep .1)
        (atomic
         (when (> (vref a i) m)
           ;(sleep .1)
           (set! m (vref a i)))))))
(when #t
  (max04)
  (displayln m))

(when #t
  (time (max01))
  (time (max02))
  (time (max03))
  (time (max04)))