#lang racket
(require racket/gui)

(define my-frame%
  (class frame%
    (define (on-close)
      (displayln "Closing the window!")
      )
    (augment on-close)
    (super-new)
    ))

(define f (new my-frame% (label "CLOSE THIS WINDOW!")
               (min-height 256)
               (min-width 256)))

(send f show #t)

