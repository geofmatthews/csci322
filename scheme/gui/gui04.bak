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

;; Let's add more controls:

(define button1 
  (new button%
       (parent frame)
       (label "Click me!")
       (callback
        (lambda (button event)
          (send frame set-status-text "You clicked me!")))))

(define button2
  (new button%
       (parent frame)
       (label "Don't click me!")
       (callback
        (lambda (button event)
          (send frame set-status-text "You FOOL!")))))

(define slider
  (new slider%
       (parent frame)
       (label "Slide me!")
       (min-value 0)
       (max-value 100)
       (callback
        (lambda (slider event)
          (send frame set-status-text
                (format "You slid me to ~a!" 
                        (send slider get-value)))))))
