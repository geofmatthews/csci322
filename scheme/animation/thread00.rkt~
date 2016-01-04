#lang racket
;; Demo of animation using threads
;; Geoffrey Matthews
;; 2013

(require racket/gui)

(define frame (instantiate frame% ("Example")))
(send frame create-status-line)

(define my-canvas%
  (class canvas% ; The base class is canvas%
    ;; Declare overrides:
    (override on-event on-paint )
    (define cubes (make-object bitmap% "cubes.jpg"))
    (define buffer-bitmap (make-object bitmap% 256 256))
    (define buffer-dc (make-object bitmap-dc% buffer-bitmap))
    
    (define rotation 0)
    (define increment 1)
    
    ;; Double buffered:
    (define (dc) buffer-dc)
    (define (copy-buffer) (send (send this get-dc)
                                draw-bitmap buffer-bitmap 0 0))
    ;; Not double buffered:
    ;(define (dc) (send this get-dc))
    ;(define (copy-buffer) #f)
    
    (define animate
      (thread
       (lambda ()
         (let loop ()
           (set! rotation (modulo (+ rotation increment) 32))
           (send this refresh)
           (sleep 0.05)
           (loop)))))
        
    ;; Define overriding method to handle mouse events
    (define on-event 
      (lambda (event)
        (when (send event button-down?)
          (set! increment (if (zero? increment) 1 0))
          ;(if (thread-running? animate)
          ;    (thread-suspend animate)
          ;    (thread-resume animate))
          )))
    
    (define on-paint
      (lambda ()
        (send (dc) draw-bitmap-section cubes
              0 0 (* rotation 256) 0 256 256)
        (copy-buffer)
        ))
    
    ;; Call the superclass initialization
    (super-new)))
  
(define panel (instantiate horizontal-panel% (frame)
                (stretchable-height #f)))

(let loop ((i 0))
  (when (< i 3)
    (instantiate button% 
      ((format "Button ~a" i)
       panel 
       (lambda (button event) 
         (send frame set-status-text (format "Button ~a click" i)))))
    (loop (+ i 1))))

(define canvas (instantiate my-canvas% (frame) 
                 (min-width 256) (min-height 256)))
  
(send frame show #t)
