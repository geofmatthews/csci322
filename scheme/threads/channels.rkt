#lang racket

(define d displayln)

(define chan (make-channel))

(define thread-a (lambda ()
                   (let ((val (channel-get chan)))
                     (d (list 'a val))
                     (sleep 1)
                     (channel-put chan (* val val)))))

(define thread-b (lambda ()
                   (sleep 1)
                   (channel-put chan 5)
                   (let ((val (channel-get chan)))
                     (d (list 'b val)))))
  
(for-each thread
          (list thread-a 
                thread-b))

(sleep 3)
