#lang racket
;; Demo of animation using threads
;; Geoffrey Matthews
;; 2013

;; This version uses one thread for all animations
(require racket/gui)

(define frame (instantiate frame% ("Example")))
(send frame create-status-line)

(define my-canvas%
  (class canvas% ; The base class is canvas%
    ;; Declare overrides:
    (override on-event on-paint )
    (define cubes (make-object bitmap% "cubes.jpg"))
    (define buffer-bitmap (make-object bitmap% (* 3 256) 256))
    (define buffer-dc (make-object bitmap-dc% buffer-bitmap))
    
    (define rotations (vector 0 8 16))
    (define increments (vector 1 1 1))
    
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
         (let outer-loop ()
           (let inner-loop ((i 0))
             (when (< i 3)
               (vector-set! 
                rotations i
                (modulo
                 (+ (vector-ref rotations i)
                    (vector-ref increments i))
                 32))
               (inner-loop (add1 i))))
           (send this refresh)
           (sleep 0.05)
           (outer-loop)))))
        
    ;; Define overriding method to handle mouse events
    (define on-event 
      (lambda (event)
        (when (send event button-down?)
          (let* ((pos (quotient (send event get-x) 256))
                 (inc (vector-ref increments pos)))
            (vector-set! increments pos (if (zero? inc) 1 0)))
          )))
    
    (define on-paint
      (lambda ()
        (send (dc) draw-bitmap-section cubes
              0 0 (* (vector-ref rotations 0) 256) 0 256 256)
        (send (dc) draw-bitmap-section cubes
              256 0 (* (vector-ref rotations 1) 256) 0 256 256)
        (send (dc) draw-bitmap-section cubes
              512 0 (* (vector-ref rotations 2) 256) 0 256 256)
        (copy-buffer)
        ))
    
    ;; Call the superclass initialization
    (super-new)))
  
(define panel (instantiate horizontal-panel% (frame)
                (stretchable-height #f)))

(let loop ((i 0))
  (when (< i 5)
    (instantiate button% 
      ((format "Button ~a" i)
       panel 
       (lambda (button event) 
         (send frame set-status-text (format "Button ~a click" i)))))
    (loop (+ i 1))))

(define canvas (instantiate my-canvas% (frame) 
                 (min-width (* 3 256)) (min-height 256)))
  
(send frame show #t)
