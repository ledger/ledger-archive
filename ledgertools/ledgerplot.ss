#!/usr/bin/env mred -f
;; plot money and time charts from ledger data with plt scheme
;; in progress

(require
 (lib "class.ss")
 (lib "mred.ss" "mred")
 (lib "plot.ss" "plot")
 (lib "plot-extend.ss" "plot")
 )

(define DATAFILE "expenses.dat")

(define (readlines filename) ; filepath -> [string]
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read-line p))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read-line p) (cons line result)))))))

(define (get-balance-fields s) ; string -> (number . string)
  (let ((p (open-input-string s)))
    (cons (string->number (substring (symbol->string (read p)) 1))
          (read p))))

(define DATA (map get-balance-fields (readlines DATAFILE))) ; [(number . string)]

;;

(define (draw-bar x-position width height view)
  (let ((x1 (- x-position (/ width 2)))
        (x2 (+ x-position (/ width 2))))
    (send view fill
          `(,x1 ,x1 ,x2 ,x2)
          `(0 ,height ,height 0))))

(define-plot-type bar-chart
  data 2dview [(color 'red) (bar-width .75)]
  (begin
    (send 2dview set-line-color color)
    (for-each
     (lambda (bar) (draw-bar (car bar) bar-width
                             (cadr bar) 2dview))
     data)))

(define (bar-chart-2)
  (bar-chart (list-of (list number number))
             [(color symbol) (bar-width number)]))
.
;;(plot (barchart-2 ... (x-min 0) (y-min 0) (x-max 6) (y-max 10)))

