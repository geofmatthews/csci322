#lang racket
(define fred (make-semaphore))
(semaphore-post fred)
(semaphore-wait fred)
(displayln 'hello)
