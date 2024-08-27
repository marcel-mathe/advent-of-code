#lang racket

(require racket/contract)
(require racket/file)

(require math/base) ; for sum/*

(require "advent-tools.rkt")
(require "struct03.rkt")

;; day 03
(define *symbol-list* '(#\* #\# #\+ #\$))
(define input (file->lines "./input03.txt"))

; create a list of all (~8) neighbours to a cell c
(define/contract (cell-neighbours row-max col-max c)
  (-> number? number? cell? (listof cell?))
  ; a valid cell lies in between the (0, row-max] and (0, col-max]
  (define/contract (cell-valid? c)
    (-> cell? boolean?)
    (let ([row (cell-row c)]
          [col (cell-col c)])
      (and (>= row 0) (< row row-max)
           (>= col 0) (< col col-max))))
  (let (; first three neighbours, above
        [n0 (cell (- (cell-row c) 1) (- (cell-col c) 1))]
        [n1 (cell (- (cell-row c) 1)    (cell-col c))]
        [n2 (cell (- (cell-row c) 1) (+ (cell-col c) 1))]
        ; adjacent neighbours
        [n3 (cell (cell-row c) (- (cell-col c) 1))]
        [n5 (cell (cell-row c) (+ (cell-col c) 1))]
        ; neigbours, down under
        [n6 (cell (+ (cell-row c) 1) (- (cell-col c) 1))]
        [n7 (cell (+ (cell-row c) 1)    (cell-col c))]
        [n8 (cell (+ (cell-row c) 1) (+ (cell-col c) 1))])
    ; fix boundary and return only "good" neighbours
    (filter cell-valid? (list n0 n1 n2 n3 n5 n6 n7 n8))))

; create a list of (reachable) neighbours for all cells in xs
(define/contract (all-neighbours row-max col-max number-coords)
  (-> number? number? (listof cell?) (listof cell?))
  (remove-duplicates
   (flatten
    (map (curry cell-neighbours row-max col-max) number-coords))))

; has a symbol one of the coords in neighbour-coords?
(define/contract (symbol-as-neighbour? symbol-coords neighbour-coords)
  (-> (listof cell?) (listof cell?) (or/c #f cell?))
  ; check for duplicates in the combined cells list
  (check-duplicates (append neighbour-coords symbol-coords)))

; parse the complete map into a list of lists of num- und sym-structs
(define/contract (parseMap m)
  (-> (listof string?) (listof (listof (or/c sym? num?))))
  (let ([row -1])
    (for/list ([line m])
      (set! row (+ 1 row))
      (parseLine row line))))

; parse a single line into a list of num- and sym-structs
(define (parseLine row line)
  (-> string? number? (listof (or/c num? sym?)))
  ; create a num struct from the digits and coords
  (define (mknumber numDigi numCoord)
    (if (empty? numDigi)
        ; if there are no digits, we return nothing
        '()
        ; create the num
        (list (num numCoord (string->number (list->string numDigi))))))
  ; recursive scan the line
  (define (f col numDigi numCoord res line)
    (if (empty-string? line)
        ; base case
        (append res (mknumber numDigi numCoord))
        ; we have something to parse
        (let ([parsedCell (parseCharacter (string-ref line 0))])
          (cond
            ; found a single digit
            [(string=? (car parsedCell) "digit")
             (f (add1 col)
                (append numDigi (list (cdr parsedCell)))
                (append numCoord (list (cell row col)))
                res
                (substring line 1))]
            ; found a symbol
            [(string=? (car parsedCell) "symbol")
             (f (add1 col)
                empty ; clear number and coord cache
                empty
                (append
                 ; add number (if any)
                 (append res (mknumber numDigi numCoord))
                 ; add found symbol
                 (list (sym (cell row col) (cdr parsedCell))))
                (substring line 1))]
            ; anything else
            [else
             (f (add1 col)
                empty ; clear number and coord cache
                empty
                (append res (mknumber numDigi numCoord))
                (substring line 1))]))))
  ; call
  (f 0 empty empty empty line))

; parse a single character on the elf map
(define/contract (parseCharacter c)
  (-> char? pair?)
  ; helper function, a symbol is a thing in our symbol list
  (define/contract (symb? c)
    (-> char? (or/c #f (listof char?)))
    (member c *symbol-list*))
  ; we have 3 options: a digit, a symbol, or a dot
  (cond
    ; single digit
    [(char-numeric? c) (cons "digit" c)]
    ; symbol
    [(symb? c) (cons "symbol" (string->symbol (list->string (list c))))]
    ; anything else
    [else (cons "dot" null)]))

; day 03 part 01
(define (day-03-part-01 lines)
  (let* (; list of anything found
         [elfmap (flatten (parseMap lines))]
         ; list of all found symbols
         [symmap (filter sym? elfmap)]
         ; list of all found numbers
         [nummap (filter num? elfmap)]
         ; how many rows has this map?
         [row-max (length lines)]
         ; how many columns has this map?
         [col-max (string-length (car lines))])
    ; sum of numbers with symbolic neighbours
    (sum
     ; extract numeric value
     (map num-value
          ; filter out numbers without neighbour
          (filter
           (λ (item)
             (curry symbol-as-neighbour?
                    ; coordinates of all symbols
                    (map sym-coord symmap)
                    ; coordinates of all (reachable) neighbours
                    (all-neighbours row-max col-max (num-cells item))))
           nummap)))))

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
       [expected 4361])
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
