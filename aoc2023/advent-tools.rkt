#lang racket

(provide print-example print-day)

(define (correct solution try)
  (cond
    [(or (null? solution) (null? try)) "ERROR"]
    [(= solution try) "(Correct!)"]
    [else "(Incorrect!)"]))

(define (print-example day ex result ok)
  (fprintf
   (current-output-port)
   "Day ~a Example ~a: ~a ~a~n~n"
   day
   ex
   result
   (correct ok result)))

(define (print-day day part result ok)
  (fprintf
   (current-output-port)
   "Day ~a Part ~a: ~a ~a~n~n"
   day
   part
   result
   (correct ok result)))
