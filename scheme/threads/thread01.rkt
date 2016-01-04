#lang racket

(define d displayln)


(lambda ()
(d "1 2 3")
(d "4 5 6")
(d "7 8 9")
)

(lambda ()
(d "hello")
(d "goodbye")
)

(lambda ()
(d (+ 234 56345))
(d (* 23 4 5 6))
(d pi)
  )