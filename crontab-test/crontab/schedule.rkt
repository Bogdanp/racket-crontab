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
          ((,(utcdate 2022 5 29 12 30 29) . ,(utcdate 2022 5 29 12 31))))

         ("*/10 * * * *"
          ((,(utcdate 2022 5 29 12 10 10) . ,(utcdate 2022 5 29 12 20))
           (,(utcdate 2022 5 29 12 15 33) . ,(utcdate 2022 5 29 12 20))))

         ("1,5 10,12 * * *"
          ((,(utcdate 2022 5 29 5 10 15) . ,(utcdate 2022 5 29 10 1))
           (,(utcdate 2022 5 29 10 7 15) . ,(utcdate 2022 5 29 12 1))
           (,(utcdate 2022 5 29 12 5) . ,(utcdate 2022 5 29 12 5))
           (,(utcdate 2022 5 29 12 6) . ,(utcdate 2022 5 30 10 1))
           (,(utcdate 2022 12 31 12 6) . ,(utcdate 2023 1 1 10 1))
           (,(utcdate 1992 2 28 12 5) . ,(utcdate 1992 2 28 12 5))
           (,(utcdate 1992 2 28 12 7) . ,(utcdate 1992 2 29 10 1))
           (,(utcdate 1991 2 28 12 5) . ,(utcdate 1991 2 28 12 5))
           (,(utcdate 1991 2 28 12 9) . ,(utcdate 1991 3 1 10 1))))

         ("* 10 1 * *"
          ((,(utcdate 2022 5 29 10 15) . ,(utcdate 2022 6 1 10))
           (,(utcdate 2022 1 31 10 15) . ,(utcdate 2022 2 1 10))))

         ("* * 31 * *"
          ((,(utcdate 2022 1 1 5 30 33) . ,(utcdate 2022 1 31))))

         ("* 10 31 * *"
          ((,(utcdate 2022 5 29 10 15) . ,(utcdate 2022 5 31 10))
           (,(utcdate 2022 1 31 11 10) . ,(utcdate 2022 3 31 10))))

         ("5 10 31 12 *"
          ((,(utcdate 2022 5 29 10 15) . ,(utcdate 2022 12 31 10 5))
           (,(utcdate 2022 12 31 11 10) . ,(utcdate 2023 12 31 10 5))))

         ("* * * 4 *"
          ((,(utcdate 2022 5 29) . ,(utcdate 2023 4 1))))

         ("30 0 31 2 5"
          ((,(utcdate 2022 8 18) . ,(utcdate 2023 2 3 0 30))
           (,(utcdate 2022 2 2) . ,(utcdate 2022 2 4 0 30))
           (,(utcdate 2022 2 4 0 31) . ,(utcdate 2022 2 11 0 30))
           (,(utcdate 2022 2 28 0 31) . ,(utcdate 2023 2 3 0 30))))))

     (for ([t (in-list tests)])
       (match-define (list schedule-str schedule-tests) t)
       (define s (parse-schedule schedule-str #f))
       (for ([(st idx) (in-indexed (in-list schedule-tests))])
         #;(println '---)
         (match-define (cons in out) st)
         (define in-date-str (date->string in #t))
         (define res-date (seconds->date (schedule-next s (date->seconds in #f)) #f))
         (define res-date-str (date->string res-date #t))
         (define out-date-str (date->string out #t))
         (check-equal? res-date-str out-date-str (format "~a # ~a (~a)" schedule-str (add1 idx) in-date-str)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests schedule-tests))
