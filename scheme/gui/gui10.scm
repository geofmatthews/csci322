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

(define h-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)
       (style '(border))
       (border 2)))

(define drawing-choice
  (new choice%
       (parent h-panel)
       (label "Draw a")
       (choices '("Circle" "Square" "Triangle"))
       (callback (lambda (c e) (send canvas refresh)))
       ))

;; Let's add our own canvas by subclassing canvas,
;; and overriding on-event as well as on-paint.
;; BUT NO PAINTING is done anywhere but on-paint!!

(define my-canvas%
  (class canvas%
    (override on-paint on-event)
    (init-field (points '()))
    (define (on-paint)
      (let ((dc (send this get-dc))
            (w (send this get-width))
            (h (send this get-height)))
        (case (send drawing-choice get-selection)
          ((0) (send dc draw-ellipse 10 10  (- w 20) (- h 20)  ))
          ((1) (send dc draw-rectangle  10 10  (- w 20) (- h 20)))
          ((2) (send dc draw-lines
                     (list
                      (make-object point% (/ w 2) 10)
                      (make-object point% (- w 10) (- h 10))
                      (make-object point% 10 (- h 10))
                      (make-object point% (/ w 2) 10)
                      ))))
        (for-each (lambda (point)
                    (send dc draw-ellipse (- (car point) 5) (- (cadr point) 5) 10 10 ))
                  points)
        ))
    (define (on-event event)
      (when (send event button-down?)
        (let ((x (send event get-x))
              (y (send event get-y)))
          (send frame set-status-text (format "Mouse at ~a ~a" x y))
          (set! points (cons (list x y) points))
          (send this refresh)
          ))
      )
    
    (super-new)))

(define canvas
  (new my-canvas%
       (parent frame)
       (style '(border))
       (min-width 300)
       (min-height 200)))
       
             
       
