#lang racket

(provide (struct-out game)
         (struct-out set))

; a game has an id and a list of sets
(struct game (id sets) #:transparent)
; a set contains the red, green and blue values
(struct set (r g b) #:transparent)
