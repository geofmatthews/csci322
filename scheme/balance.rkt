#lang racket

(define mutex (make-semaphore 1))

(define balance 0)

(define add
  (lambda (balance amt)
    (+ balance amt)))

(define deposit
  (lambda (amount)
    (for ((i (in-range amount)))
      ;(semaphore-wait mutex)
      (let ((local balance))
        (set! balance (add local 1)))
      ;(semaphore-post mutex)
      )))

(for ((i (in-range 1000)))
  (thread
   (lambda ()
     (deposit 1000))))

(let loop ((b balance))
  (sleep 1)
  (when (not (= b balance))
    (displayln balance)
    (sleep 1)
    (loop balance)))
(displayln (format "Final balance: ~a" balance))