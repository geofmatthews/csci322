#lang racket
;; Geoffrey Matthews
;; 2013

;; Building gui's with racket
(require racket/gui)

(define frame (new frame% 
                   (label "Example")
                   (min-width 120)
                   (min-height 80)
                   ))
(send frame create-status-line)
(send frame show #t)

;; Let's add a control:

(define button1 
  (new button%
       (parent frame)
       (label "Click me!")
       (callback
        (lambda (button event)
          (send frame set-status-text "You clicked me!")))))
