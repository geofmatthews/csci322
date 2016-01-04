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

;; Let's add lots of controls, laid out horizontally
;; and then vertically:

(define h-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)
       (style '(border))
       (border 2)))

(let loop ((i 0))
  (when (< i 10) 
    (new button% 
         (parent h-panel)
         (label (format "Button~a" i))
         (callback 
          (lambda (b e)
            (send frame set-status-text
                  (format "You clicked horizontal button ~a!" i)))))
    (loop (add1 i))))
    

(define v-panel
  (new vertical-panel%
       (parent frame)
       (style '(border))
       (border 2)))
                                  
(let loop ((i 0))
  (when (< i 10) 
    (new button% 
         (parent v-panel)
         (label (format "Button~a" i))
         (callback 
          (lambda (b e)
            (send frame set-status-text
                  (format "You clicked vertical button ~a!" i)))))
    (loop (add1 i))))
