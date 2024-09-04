#lang racket

(require racket/file)
(require racket/string)

(require "advent-tools.rkt")

;; day 01
(define input (file->lines "./input01.txt"))

; get a list of the first and the last digit
(define (combine-first-last-digits str)
  (let ([digits (list->string (filter char-numeric? (string->list str)))])
    (if (empty-string? digits)
        ; no digits at all, early exit
        '(0)
        ; everything else
        (let*
            ; get [first|last] digit according to fun from str
            ([digi (λ (fun str)
                     (if (non-empty-string? str)
                         ; some digits
                         (string->number
                          (list->string
                           (list (fun (string->list str)))))
                         ; no digits
                         0))]
             ; first digit, times 10 for summation
             [first_digit (* 10 (digi first digits))]
             ; last digit
             [last_digit (digi last digits)])
          (list first_digit last_digit)))))

; replace all worded digits to number digits
; from the left to the right
(define (replace-word-digits str)
  (letrec
      ([f (λ (acc s)
            (if (empty-string? s)
                ; base case
                acc
                ; everything else
                (let ([new-acc 
                       (cond
                         ; replace worded digit
                         [(string-prefix? s "one") (string-append acc "1")]
                         [(string-prefix? s "two") (string-append acc "2")]
                         [(string-prefix? s "three") (string-append acc "3")]
                         [(string-prefix? s "four") (string-append acc "4")]
                         [(string-prefix? s "five") (string-append acc "5")]
                         [(string-prefix? s "six") (string-append acc "6")]
                         [(string-prefix? s "seven") (string-append acc "7")]
                         [(string-prefix? s "eight") (string-append acc "8")]
                         [(string-prefix? s "nine") (string-append acc "9")]
                         ; no digit found, step one character right
                         ; and keep the old character
                         [else (string-append acc (substring s 0 1))])])
                  (f new-acc (substring s 1)))))])
    (f "" str)))

; day 01 part 01
; sum of all sum of first and last digit of a line
(define (day-01-part-01 lines)
  (for/fold ([sum 0])
            ([line (in-list lines)])
    ; add all the line values together
    (+ sum (apply + (combine-first-last-digits line)))))

; day 01 part 02
; same as in part 01, but some digits are written as words
(define (day-01-part-02 lines)
  (for/fold ([sum 0])
            ([line (in-list lines)])
    (+ sum
       (apply +
              (combine-first-last-digits
               (replace-word-digits line))))))

; example 01
(let* ([ex01 '("1abc2"
               "pqr3stu8vwx"
               "a1b2c3d4e5f"
               "treb7uchet")]
       [result (day-01-part-01 ex01)]
       [expected 142])
  (print-example 1 1 result expected))

; part 01
(let ([result (day-01-part-01 input)]
      [expected 54390])
  (print-day 1 1 result expected))

; example 02
(let* ([ex02 '("two1nine"
               "eightwothree"
               "abcone2threexyz"
               "xtwone3four"
               "4nineeightseven2"
               "zoneight234"
               "7pqrstsixteen")]
       [result (day-01-part-02 ex02)]
       [expected 281])
  (print-example 1 2 result expected))

; part 02
(let ([result (day-01-part-02 input)]
      [expected 54277])
  (print-day 1 2 result expected))
