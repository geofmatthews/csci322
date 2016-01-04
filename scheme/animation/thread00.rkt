#lang racket
;; Demo of animation using threads
;; Geoffrey Matthews
;; 2013

(require racket/gui)

(define frame (instantiate frame% ("Example")))
(send frame create-status-line)

(define panel (instantiate horizontal-panel% (frame)
                (stretchable-height #f)))

(instantiate button% 
  ("Stop/Start Counter"
   panel 
   (lambda (button event) 
     (if (thread-running? counter)
         (thread-suspend counter)
         (thread-resume counter))
     (send frame set-status-text "Clicked"))))

(define my-text (new text-field% (label "N:") (parent frame) 
                  (min-width 256) (min-height 256)))

(define counter
  (let ((n 0))
    (thread
     (lambda ()
       (let loop ()
         (set! n (+ n 1))
         (send (send my-text get-editor) insert (format "~a\n" n))
         (sleep 0.5)
         (loop)))))  )

(send frame show #t)
