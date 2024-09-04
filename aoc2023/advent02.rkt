#lang racket

(require racket/contract)
(require racket/file)
(require racket/string)

(require megaparsack megaparsack/text)

(require "advent-tools.rkt")
(require "struct02.rkt")
(require "parser02.rkt")

;; day 02
(define input (file->lines "./input02.txt"))
(define *limit-red* 12)
(define *limit-green* 13)
(define *limit-blue* 14)

; does the set fall inside the limits?
(define/contract (in-rgb-limits? s)
  (-> set? boolean?)
  (and (<= 0 (set-r s) *limit-red*)
       (<= 0 (set-g s) *limit-green*)
       (<= 0 (set-b s) *limit-blue*)))

; is the game possible?
(define/contract (possible-game? g)
  (-> game? boolean?)
  (andmap in-rgb-limits? (game-sets g)))

; input -> list of games
(define/contract (parse-input ls)
  (-> (listof string?) (listof game?))
  (map (λ (l) (parse-result! (parse-string game/p l))) ls))

; day 02 part 01
; sum of the ids of all possible games
(define/contract (day-02-part-01 lines)
  (-> (listof string?) number?)
  (foldl +
         0
         (map game-id
              (filter possible-game? (parse-input lines)))))

; maximal number of cubes (per color) in this game
(define/contract (max-color-per-game g)
  (-> game? (listof integer?))
  (list (apply max (map set-r (game-sets g)))
        (apply max (map set-g (game-sets g)))
        (apply max (map set-b (game-sets g)))))

; day 02 part 02
; (-> (listof string?) number?)
(define (day-02-part-02 lines)
  (foldl (λ (g acc) (+ (apply * (max-color-per-game g)) acc))
         0
         (parse-input lines)))

; example 01
(let* ([ex01 (list
              "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
              "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
              "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
              "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
              "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")]
       [result (day-02-part-01 ex01)]
       [expected 8])
  (print-example 2 1 result expected))

; part 01
(let ([result (day-02-part-01 input)]
      [expected 3059])
  (print-day 2 1 result expected))

; example 02
(let* ([ex02 (list
              "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
              "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
              "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
              "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
              "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")]
       [result (day-02-part-02 ex02)]
       [expected 2286])
  (print-example 2 2 result expected))

; part 02
(let ([result (day-02-part-02 input)]
      [expected 65371])
  (print-day 2 2 result expected))
