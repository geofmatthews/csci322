#lang racket
;; Geoffrey Matthews
;; 2013

;; Building gui's with racket
(require racket/gui)

;; Subclassing

;; Remember our simple object:

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
    
    ;; Each class must initialize its superclass:
    (super-new)
    ))

;; Define a new subclass that inherits everything:

(define my-sub-object%
  (class my-object%
    ;; new field
    (init-field (another-number 200))
    ;; inherit superclass fields:
    (inherit-field a-number)
    ;; override superclass methods:
    (override get-number set-number!)
    
    (define (get-number)
      (+ a-number another-number))
    (define (set-number! n)
      (set! a-number n)
      (set! another-number (* 2 n)))
    
    (super-new)))

(define a (new my-object%))
(define b (new my-sub-object%))

;; b acts just like a for things we don't override:
(print
 (list (send a get-string)
       (send b get-string)))
(newline)

;; but b acts differently for things we override:
(send a set-number! 200)
(send b set-number! 200)
(print
 (list (send a get-number)
       (send b get-number)))
(newline)
    