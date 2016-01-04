#lang racket

;; Define, then use:
(define (loop n)
  (if (zero? n)
      (print 'done)
      (begin
        (print n)
        (loop (- n 1)))))

(loop 5)

;; Define and use in a single expression:
(let loop ((n 5))
  (if (zero? n)
      (print 'done)
      (begin
        (print n)
        (loop (- n 1)))))