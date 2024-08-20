#lang racket
(require racket/file)
(require racket/string)
(require "advent-tools.rkt")

;; day xx
(define input (file->lines "./inputXX.txt"))

; day xx part 01
(define (day-XX-part-01 lines) '())

; day xx part 02
(define (day-XX-part-02 lines) '())

; example 01
(let* ([ex01 '()]
       [result (day-XX-part-01 ex01)]
       [expected -1])
  (print-example XX 1 result expected))

; part 01
(let ([result (day-XX-part-01 input)]
      [expected -1])
  (print-day XX 1 result expected))

; example 02
(let* ([ex02 '()]
       [result (day-XX-part-02 ex02)]
       [expected -1])
  (print-example XX 2 result expected))

; part 02
(let ([result (day-XX-part-02 input)]
      [expected -1])
  (print-day XX 2 result expected))
