#lang racket

(define vector-size 1000)
(define procs (make-vector vector-size))
(define values (make-vector vector-size))
(define loop-limit 10000)

(define proc (lambda (i)
               (lambda () 
                 (vector-set! values i 0)
                 (do ((n 0 (+ n 1)))
                   ((> n loop-limit))
                   (vector-set! values i (+ (vector-ref values i) n)) ))))

(define start (current-milliseconds))

(do ((n 0 (+ n 1)))
  ((>= n vector-size))
  (vector-set! procs n (proc n)))
  
(do ((n 0 (+ n 1)))
  ((>= n vector-size))
  ((vector-ref procs n)))
  
(displayln (vector-ref values 0))
(displayln (vector-ref values (- vector-size 1)))

(define mid (current-milliseconds))

(do ((n 0 (+ n 1)))
  ((>= n vector-size))
  (vector-set! procs n (future (proc n))))
  
(do ((n 0 (+ n 1)))
  ((>= n vector-size))
  (touch (vector-ref procs n))
  )
  
(displayln (vector-ref values 0))
(displayln (vector-ref values (- vector-size 1)))

(define end (current-milliseconds))

(display "Time taken without futures: ")
(displayln (- mid start))
(display "Time taken with futures:    ")
(displayln (- end mid))