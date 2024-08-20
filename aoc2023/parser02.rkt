#lang racket

(provide game/p)

(require data/monad)
(require data/applicative)
(require megaparsack megaparsack/text)

(require "struct02.rkt")

; parser for a single color entry (with maximal two digits)
(define mk-color/p (λ (symbol)
  (do [x <- (many/p integer/p #:sep void/p #:min 1 #:max 2)]
    space/p
    (string/p (symbol->string symbol))
    (pure (cons symbol (car x))))))

; specific colors
(define red/p (mk-color/p 'red))
(define green/p (mk-color/p 'green))
(define blue/p (mk-color/p 'blue))

; a single color in a set
(define single-color/p
  (or/p (try/p red/p)
        (try/p green/p)
        blue/p))

; find a single set entry
(define set/p
  (do [dict <- (many+/p single-color/p #:sep (string/p ", ") #:max 3)]
    (pure (set (dict-ref dict 'red 0)
               (dict-ref dict 'green 0)
               (dict-ref dict 'blue 0)))))

; find a list of sets
(define sets/p
  (many+/p set/p #:sep (string/p "; ")))

; a single game id (with maximal 3 digits)
(define game-id/p
  (many+/p integer/p #:max 3))

; a complete line
(define game/p
  (do (string/p "Game ")
    [id <- game-id/p]
    (string/p ": ")
    [sets <- sets/p]
    (pure (game (car id) sets))))
