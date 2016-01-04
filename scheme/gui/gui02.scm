#lang racket
;; Geoffrey Matthews
;; 2013

;; Building gui's with racket
(require racket/gui)

(define frame (new frame% 
                   (label "Example")
                   (min-width 100)
                   (min-height 80)
                   ))
(send frame show #t)
