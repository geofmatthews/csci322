#lang racket

(define balance 0)

(define deposit 
  (lambda ()
    (sleep (random))
    (let ((b balance))
      (sleep (random))
      (set! balance (+ b 1)))))

(for ((n (in-range 10)))
  (thread deposit))

(sleep 5)
(displayln balance)