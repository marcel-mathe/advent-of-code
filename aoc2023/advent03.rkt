#lang racket

(require racket/file)
(require racket/string)

(require "advent-tools.rkt")

;; day 03
(define input (file->lines "./input03.txt"))

; day 03 part 01
(define (day-03-part-01 lines) '())

; day 03 part 02
(define (day-03-part-02 lines) '())

; example 01
(let* ([ex01 (list "467..114.."
                   "...*......"
                   "..35..633."
                   "......#..."
                   "617*......"
                   ".....+.58."
                   "..592....."
                   "......755."
                   "...$.*...."
                   ".664.598..")]
       [result (day-03-part-01 ex01)]
       [expected -1])
  (print-example 3 1 result expected))

; part 01
(let ([result (day-03-part-01 input)]
      [expected -1])
  (print-day 3 1 result expected))

; example 02
(let* ([ex02 '()]
       [result (day-03-part-02 ex02)]
       [expected -1])
  (print-example 3 2 result expected))

; part 02
(let ([result (day-03-part-02 input)]
      [expected -1])
  (print-day 3 2 result expected))
