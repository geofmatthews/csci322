#lang racket

(require racket/block)

(define sem (make-semaphore))
(define thread-a (lambda () 
                   (displayln 'a1)
                   (semaphore-post sem)
                   ))
(define thread-b (lambda ()
                   (semaphore-wait sem)
                   (displayln 'b1)
                   ))

(block 
 (thread thread-b)
 (sleep 1)
 (thread thread-a)
 (displayln 'done))