#lang racket
;; Geoffrey Matthews
;; 2013

;; Building gui's with racket
(require racket/gui)

;; demo of DrScheme's objects

;; Simple object:

(define my-object%
  (class object%
    ;; public methods:
    (public get-number get-string set-number! set-string!)
    ;; private data:
    (init-field (a-number 99) (a-string "Hello")) 
    
    ;; define whatever procedures you want here,
    ;; only the "public" ones declared above will be 
    ;; available:
    
    (define (get-number) a-number)
    (define (get-string) a-string)
    (define (set-number! n) (set! a-number n))
    (define (set-string! s) (set! a-string s))
    
    ;(set! a-string "xxxxxxxx")
    
    ;; Each class must initialize its superclass:
    (super-new)
    ))

;; 3 ways to make objects, which are instances of a class:

(define a (make-object my-object% 1))
(define b (new my-object% (a-string "Goodbye")))
(define c (instantiate my-object% (12 "Whaa?")))
(define d (instantiate my-object% (3) (a-string "fun")))

;; sending messages to objects:

(print
 (+ (send a get-number) (send b get-number) 
    (send c get-number) (send d get-number)))
(newline)

(print
 (list
  (send a get-string)
  (send b get-string)
  (send c get-string)
  (send d get-string))
)
(newline)

(send a set-string! "Foo!")
(send b set-string! "Blarf")

(print
 (list
  (send a get-string)
  (send b get-string)
  (send c get-string)
  (send d get-string))
)
(newline)
