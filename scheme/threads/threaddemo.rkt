#lang racket

(define d displayln)

(define (run thunk) (thunk))

(define thunk-a (lambda ()
                   (d 'a1)
                   (d 'a2)
                   (d 'a3)))

(define thunk-b (lambda ()
                   (d 'b1)
                   (d 'b2)))

(define thunk-gen-c (lambda (x y z)
                      (lambda ()
                        (d (list 'c1 x y z))
                        (d (list 'c2 x y z)))))

(define thunks
  (list thunk-a 
        thunk-b
        (thunk-gen-c 1 2 3)
        (thunk-gen-c 4 5 6)
        ))
