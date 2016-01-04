#lang racket

(require racket/block)

(provide makev vset! vref
         for atomic co await)

(define *andrews-mutex* (make-semaphore 1))
; utilities to make the array demos simpler
(define (makev n) (make-vector (+ n 1)))
(define vset! vector-set!)
(define vref vector-ref)

; concurrent primitives from Andrews "Foundations of Multithreaded ..."
(define-syntax for
  (syntax-rules ()
    ((for i from a to b exp ...)
     (let loop ((i a))
       (when (<= i b)
         exp ...
         (loop (+ i 1)))))))

(define-syntax atomic
  (syntax-rules ()
    ((atomic exp ...)
     (block
      (semaphore-wait *andrews-mutex*)
      exp ...
      (semaphore-post *andrews-mutex*)))))

(define-syntax co
  (syntax-rules ()
    ((co for i from a to b exp ...)
     (let ((threads (map (lambda (i) (thread (lambda () exp ...)))
                          (stream->list (in-range a (+ b 1))))))
       (let loop () (when (ormap thread-running? threads)
                      (sleep 0.01)
                      (loop)))
       (values)
     ))
    ((co exp ...)
     (let ((threads 
            (list
             (thread (lambda () exp))
             ...)))
       (let loop () (when (ormap thread-running? threads)
                      (sleep 0.01)
                      (loop)))
       (values)))))

(define-syntax await
  (syntax-rules ()
    ((await b exp ...)
     (block
      (semaphore-wait *andrews-mutex*)
      (do ()
        (b exp ... (semaphore-post *andrews-mutex*))
        (semaphore-post *andrews-mutex*)
        (sleep 0.01)
        (semaphore-wait *andrews-mutex*))))))

