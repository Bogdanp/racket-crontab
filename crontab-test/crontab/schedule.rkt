#lang racket/base

(require crontab/schedule
         racket/date
         racket/match
         rackunit)

(define (utcdate year month day [hours 0] [minutes 0] [seconds 0])
  (seconds->date (find-seconds seconds minutes hours day month year #f) #f))

(define schedule-tests
  (test-suite
   "schedule-tests"

   (test-suite
    "validation"

    (test-case "unsatisfiable schedules are detected"
      (for ([e (in-list '("* * 31 6 *"
                          "* * 31 6,9 *"
                          "* * 30 2 *"))])
        (check-exn
         #rx"unsatisfiable day & month constraints"
         (λ () (parse-schedule e))
         e)))

    (test-case "ranges get validated"
      (check-exn
       #rx"expected a value between 0 and 23"
       (λ () (parse-schedule "* 44 * * *")))))

   (test-case "scheduling"
     (define tests
       `(("* * * * *"
          (((2022 5  29 12 30 29) . (2022 5  29 12 31 0 ))))

         ("*/10 * * * *"
          (((2022 5  29 12 10 10) . (2022 5  29 12 20 0 ))
           ((2022 5  29 12 15 33) . (2022 5  29 12 20 0 ))))

         ("1,5 10,12 * * *"
          (((2022 5  29 5  10 15) . (2022 5  29 10 1  0 ))
           ((2022 5  29 10 7  15) . (2022 5  29 12 1  0 ))
           ((2022 5  29 12 5  0 ) . (2022 5  29 12 5  0 ))
           ((2022 5  29 12 6  0 ) . (2022 5  30 10 1  0 ))
           ((2022 12 31 12 6  0 ) . (2023 1  1  10 1  0 ))
           ((1992 2  28 12 5  0 ) . (1992 2  28 12 5  0 ))
           ((1992 2  28 12 7  0 ) . (1992 2  29 10 1  0 ))
           ((1991 2  28 12 5  0 ) . (1991 2  28 12 5  0 ))
           ((1991 2  28 12 9  0 ) . (1991 3  1  10 1  0 ))))

         ("* 10 1 * *"
          (((2022 5  29 10 15 0 ) . (2022 6  1  10 0  0 ))
           ((2022 1  31 10 15 0 ) . (2022 2  1  10 0  0 ))))

         ("* * 31 * *"
          (((2022 1  1  5  30 33) . (2022 1  31 0  0  0 ))))

         ("* 10 31 * *"
          (((2022 5  29 10 15 0 ) . (2022 5  31 10 0  0 ))
           ((2022 1  31 11 10 0 ) . (2022 3  31 10 0  0 ))))

         ("5 10 31 12 *"
          (((2022 5  29 10 15 0 ) . (2022 12 31 10 5  0 ))
           ((2022 12 31 11 10 0 ) . (2023 12 31 10 5  0 ))))

         ("* * * 4 *"
          (((2022 5  29 0  0  0 ) . (2023 4  1  0  0  0 ))))

         ("30 0 31 2 5"
          (((2022 8  18 0  0  0 ) . (2023 2  3  0  30 0 ))
           ((2022 2  2  0  0  0 ) . (2022 2  4  0  30 0 ))
           ((2022 2  4  0 31  0 ) . (2022 2  11 0  30 0 ))
           ((2022 2  28 0 31  0 ) . (2023 2  3  0  30 0 ))))))

     (for ([t (in-list tests)])
       (match-define (list schedule-str schedule-tests) t)
       (define s (parse-schedule schedule-str #f))
       (for ([(st idx) (in-indexed (in-list schedule-tests))])
         #;(println '---)
         (match-define (cons in-args out-args) st)
         (define in (apply utcdate in-args))
         (define out (apply utcdate out-args))
         (define in-date-str (date->string in #t))
         (define res-date (seconds->date (schedule-next s (date->seconds in #f)) #f))
         (define res-date-str (date->string res-date #t))
         (define out-date-str (date->string out #t))
         (check-equal? res-date-str out-date-str (format "~a # ~a (~a)" schedule-str (add1 idx) in-date-str)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests schedule-tests))
