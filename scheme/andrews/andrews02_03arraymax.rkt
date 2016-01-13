#lang racket
(require "andrews.rkt")

(define n 100)
(define sleeptime 0.01)

(define a (makev n))
(for i from 0 to n (vset! a i 
                          (random 1000)
                          ))

(define m 0)

(define (max01)
  (set! m 0)
  (for i from 0 to (- n 1)
   ; (sleep sleeptime)
    (when (> (vref a i) m)
      (sleep sleeptime)
      (set! m (vref a i)))))

(define (max02)
  (set! m 0)
  (co for i from 0 to (- n 1)
     ; (sleep sleeptime)
      (when (> (vref a i) m)
        (sleep sleeptime)
        (set! m (vref a i)))))

(define (max03)
  (set! m 0)
  (co for i from 0 to (- n 1)
     ; (sleep sleeptime)
      (atomic
       (when (> (vref a i) m)
         (sleep sleeptime)
         (set! m (vref a i))))))

(define (max04)
  (set! m 0)
  (co for i from 0 to (- n 1)
     ; (sleep sleeptime)
      (when (> (vref a i) m)
           (sleep sleeptime)
        (atomic
         (when (> (vref a i) m)
           (set! m (vref a i)))))))

(for-each (lambda (max) (time (begin (max) (displayln m))))
          (list max01 max02 max03 max04))
