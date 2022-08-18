#lang racket/base

(provide
 leap-year?
 get-month-durations
 get-month-durations*

 +month
 +day
 +days
 +hour
 +minute)

(define (leap-year? year)
  (or (zero? (modulo year 400))
      (and (zero? (modulo year 4))
           (not (zero? (modulo year 100))))))

(define (get-month-durations year)
  `(31 ,(if (leap-year? year) 29 28) 31 30 31 30 31 31 30 31 30 31))

(define (get-month-durations* month)
  (case month
    [(1 3 5 7 8 10 12) '(31)]
    [(2) '(28 29)]
    [else '(30)]))

(define (+month year month)
  (if (= month 12)
      (values (add1 year) 1)
      (values year (add1 month))))

(define (+day year month day)
  (define duration
    (list-ref (get-month-durations year) (sub1 month)))
  (if (= duration day)
      (if (= month 12)
          (values (add1 year) 1 1)
          (values year (add1 month) 1))
      (values year month (add1 day))))

(define (+days year month day n)
  (for/fold ([y year] [m month] [d day])
            ([_ (in-range n)])
    (+day year month day)))

(define (+hour year month day hour)
  (cond
    [(= hour 23)
     (define-values (year* month* day*)
       (+day year month day))
     (values year* month* day* 0)]
    [else
     (values year month day (add1 hour))]))

(define (+minute year month day hour minute)
  (cond
    [(= minute 59)
     (define-values (year* month* day* hour*)
       (+hour year month day hour))
     (values year* month* day* hour* 0)]
    [else
     (values year month day hour (add1 minute))]))
