#lang racket

(require racket/struct)

(provide (struct-out cell)
         (struct-out num)
         (struct-out sym))

; helper functions for gen:equal+hash for points
(define (cell=? cell1 cell2 recursive-equal?)
  (and (= (cell-row cell1)
          (cell-row cell2))
       (= (cell-col cell1)
          (cell-col cell2))))

(define (cell-hash-code cell recursive-equal-hash)
  (+ (* 100 (cell-row cell))
     (* 1   (cell-col cell))))

(define (cell-secondary-hash-code cell recursive-equal-hash)
  (+ (* 100 (cell-row cell))
     (* 1   (cell-col cell))))

; simple row:col coordinates
(struct cell (row col)
  #:guard (λ (r c name)
            (unless (and (exact-integer? r) (exact-integer? c))
              (error "Row and col must be integers."))
            (values r c))
  #:methods gen:equal+hash
  [(define equal-proc cell=?)
   (define hash-proc  cell-hash-code)
   (define hash2-proc cell-secondary-hash-code)]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'cell)
      (lambda (obj) (list (cell-row obj) (cell-col obj)))))])

; a single found number,
; consisting of a list of cells, where to find the single digits,
; and the numeric value
(struct num (cells value)
  #:transparent
  #:guard (λ (c v name)
            (unless (and (exact-nonnegative-integer? v) (>= v 0))
              (error "Value must be a number greater or equal zero."))
            (unless ((listof cell?) c)
              (error "Cells must be a list of cells."))
            (values c v)))

; a single found symbol,
; consisting of a point and the symbol value
(struct sym (coord value)
  #:transparent
  #:guard (λ (c v name)
            (unless (symbol? v)
              (error "Value must be a symbol."))
            (unless (cell? c)
              (error "Coord must be a cell."))
            (values c v)))
