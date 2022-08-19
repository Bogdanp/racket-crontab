#lang racket/base

(require crontab/private/date
         crontab/schedule
         rackcheck
         racket/date
         racket/match
         racket/string
         rackunit)

(define (gen:range-constraint lo hi [step 1])
  (gen:let ([rng-lo (gen:integer-in lo hi)]
            [rng-hi (gen:integer-in rng-lo hi)])
    (format "~a-~a/~a" rng-lo rng-hi step)))

(define (gen:constraint lo hi [lists? #t])
  (define types
    (let ([types '(* single range range/step)])
      (if lists? (cons 'list types) types)))
  (gen:let ([type (gen:one-of types)]
            [constraint (case type
                          [(*)
                           (gen:const "*")]
                          [(single)
                           (gen:map (gen:integer-in lo hi) number->string)]
                          [(range)
                           (gen:range-constraint lo hi)]
                          [(range/step)
                           (gen:let ([step (gen:integer-in lo hi)]
                                     [constraint (gen:range-constraint lo hi (if (zero? step) 1 step))])
                             constraint)]
                          [(list)
                           (gen:map (gen:list
                                     #:max-length 3
                                     (gen:delay (gen:constraint lo hi #f)))
                                    (λ (constraints)
                                      (if (null? constraints)
                                          "*"
                                          (string-join constraints ","))))])])
    constraint))

(define gen:day&month&year
  (gen:let ([day (gen:integer-in 1 31)]
            [month (gen:integer-in 1 12)]
            [year (gen:integer-in 1970 2030)])
    (define durations (get-month-durations year))
    (define month-duration (list-ref durations (sub1 month)))
    (define day* (if (> day month-duration) month-duration day))
    (list day* month year)))

(define gen:timestamp
  (gen:let ([second (gen:integer-in 0 59)]
            [minute (gen:integer-in 0 59)]
            [hour (gen:integer-in 0 23)]
            [d&m&y gen:day&month&year])
    (apply find-seconds second minute hour d&m&y)))

(define gen:schedule
  (gen:let ([d&m&y gen:day&month&year]
            [minutes (gen:constraint 0 59)]
            [hours (gen:constraint 0 23)]
            [days (gen:constraint 1 (car d&m&y))]
            [months (gen:constraint 1 (cadr d&m&y))]
            [week-days (gen:constraint 1 7)])
    (define schedule-str
      (format "~a ~a ~a ~a ~a" minutes hours days months week-days))
    (parse-schedule schedule-str)))

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

   (test-case "scheduling (example tests)"
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
         (check-equal? res-date-str out-date-str (format "~a # ~a (~a)" schedule-str (add1 idx) in-date-str)))))

   (test-case "scheduling is gapless"
     (check-property
      (make-config #:tests (if (getenv "PLT_PKG_BUILD_SERVICE") 5 50))
      (property ([s gen:schedule]
                 [ts gen:timestamp])
        (define next-ts (schedule-next s ts))
        (define start-ts
          (let ([ts (+ ts 60)])
            (- ts (modulo ts 60))))
        #;(println `(,(schedule->string s) ,(quotient (- next-ts start-ts) 60)))
        (for ([ts (in-range start-ts next-ts 60)])
          (check-false (schedule-matches? s ts))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests schedule-tests))
