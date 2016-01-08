#lang racket
;; Geoffrey Matthews
;; 2016
;; A non-threaded boids demo

(require racket/gui)

;; Small 2d vector library for the Newtonian physics
(define (x v) (vector-ref v 0))
(define (y v) (vector-ref v 1))
(define (x! v value) (vector-set! v 0 value))
(define (y! v value) (vector-set! v 1 value))
(define (v* v value) (vector-map (lambda (x) (* x value)) v))
(define (v*! v value) (vector-map! (lambda (x) (* x value)) v))
(define (v+ v w) (vector-map + v w))
(define (v- v w) (vector-map - v w))
(define (v+! v w) (vector-map! + v w))
(define (v-zero! v) (vector-map! (lambda (x) 0) v))
(define (v-dot v w) (let ((vw (vector-map * v w))) (+ (x vw) (y vw))))
(define (v-mag v) (sqrt (v-dot v v)))
(define (perp v) (vector (- (y v)) (x v)))

;; Boid object
(define boid%
  (class object%
    (public p v calculate-force move draw)
    (init-field (screen-width 1024)
                (screen-height 768)
                (position (vector 0 0 ))
                (velocity (vector 0 0 ))
                (force (vector 0 0 ))
                (separation (vector 0 0))
                (alignment (vector 0 0))
                (cohesion (vector 0 0)))
    (define (p) position)
    (define (v) velocity)
    ;; Boids, as explained by Reynolds
    (define (calculate-force list-of-boids)
      (let ((neighborhood-radius 200.0)
            (separation-strength 10)
            (alignment-strength 10)
            (cohesion-strength 10.2)
            (max-force 0.05)
            )
        (v-zero! force)
        (v-zero! separation)
        (v-zero! alignment)
        (v-zero! cohesion)
        (for-each (lambda (other-boid)
                    (when (not (equal? this other-boid))
                      (let* ((direction (v- (send other-boid p) position))
                             (dist (v-mag direction))
                             (n 0))
                        (when (< dist neighborhood-radius)
                          (v+! separation (v* direction (* (/ -1.0 (max 1.0 dist)))))
                          (v+! alignment (send other-boid v))
                          (v+! cohesion direction)))))
                  list-of-boids)
        (v*! separation (/ separation-strength (max 1.0 (v-mag separation))))
        (v*! alignment (/ alignment-strength (max 1.0 (v-mag alignment))))
        (v*! cohesion (/ cohesion-strength (max 1.0 (v-mag cohesion))))
        (v+! force (v+ (v- separation (send this v))
                       (v+ (v- alignment (send this v))
                           (v- cohesion (send this v)))))
        (let ((force-magnitude (v-mag force)))
          (when (> force-magnitude max-force)
            (set! force (v* force
                            (/ max-force force-magnitude)))))
        ))                                
    
    ;; Simple Euler integration of acceleration and velocity
    (define (move) 
      (vector-map! + velocity force)
      (when (or (zero? (x velocity)) (zero? (y velocity)))
        (x! velocity (random))
        (y! velocity (random)))
      (vector-map! + position velocity)
      ;; Wrap the screen
      (x! position (modulo (round (x position)) screen-width))
      (y! position (modulo (round (y position)) screen-height))
      )
    ;; Draw a boid 
    (define (draw dc) 
      (send dc set-brush brush)
      (send dc set-pen pen)
      (let* ((velocity-mag (max 0.00001 (v-mag velocity)))
             (tip-vector (v* velocity (/ arrow-length velocity-mag)))
             (tip (v+ position tip-vector))
             (perp (v* (vector (- (y tip-vector)) (x tip-vector)) 0.25))
             (p1 (v+ position perp))
             (p2 (v+ position (v* perp -1)))
             (points (list (cons (x tip) (y tip))
                           (cons (x p1) (y p1))
                           (cons (x p2) (y p2)))))
        (send dc draw-polygon points)))
    
    ;; Initialize 
    (x! velocity (* 20 (- 0.5 (random))))
    (y! velocity (* 20 (- 0.5 (random))))
    (define arrow-length 20)
    ;(define color 
    ;  (let* ((r (random))
    ;         (b (real->floating-point-bytes r 4)))
    ;    (make-object color% (bytes-ref b 0) (bytes-ref b 1) (bytes-ref b 2) )))
    (define color (make-object color% 0 0 64))
    (define brush (make-object brush% color))
    (define pen (make-object pen% color))
    ;; Don't forget the super-new!
    (super-new)
    ))
;; Abstract the list-handling for a list of boids
(define boid-container%
  (class object%
    (public add-boid calculate-force move draw get-boids reset)
    (init-field (boids '()))
    (define (get-boids) boids)
    (define (reset) (set! boids '()))
    (define (add-boid boid)
      (set! boids (cons boid boids)))
    (define (calculate-force)
      (for-each (lambda (boid)
                  (send boid calculate-force boids))
                boids))
    (define (move)
      (for-each (lambda (boid)
                  (send boid move))
                boids))
    (define (draw dc)
      (for-each (lambda (boid)
                  (send boid draw dc))
                boids))
    (super-new)
    )
  )
(define boid-container (new boid-container%))

;; The GUI
(define frame (new frame% 
                   (label "boids")
                   (width 1024)
                   (height 768)
                   ))
(send frame create-status-line)
(send frame show #t)

(define h-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)
       (style '(border))
       (border 2)))

(define run-checkbox
  (new check-box%
       (parent h-panel)
       (label "Run animation")
       ))
(define reset-button
  (new button%
       (parent h-panel)
       (label "Reset")
       (callback
        (lambda (b e)
          (send boid-container reset)))))

(define my-canvas%
  (class canvas%
    (override on-paint on-event)
    
    (define (on-paint)
      (let ((dc (send this get-dc))
            (w (send this get-width))
            (h (send this get-height)))
        (send dc clear)
        (send boid-container draw dc)
        ))
    (define (on-event event)
      (when (send event button-down?)
        (let ((x (send event get-x))
              (y (send event get-y)))
          (send frame set-status-text (format "Mouse at ~a ~a" x y))
          (send boid-container add-boid (new boid% 
                                             (screen-width (send this get-width))
                                             (screen-height (send this get-height))
                                             (position (vector x y))))
          (send this refresh)
          ))
      )
    (super-new)
    (send (send this get-dc) set-background (make-object color% 192 224 255))
    ))

(define canvas
  (new my-canvas%
       (parent frame)
       (style '(border))))

;; Busy loop boid animator
(let loop ()
  (sleep/yield .025)
  (when (send run-checkbox get-value)
    (send boid-container calculate-force)
    (send boid-container move)
    (send canvas refresh)
    )
  (loop))
