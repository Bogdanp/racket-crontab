#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/date
         racket/list
         racket/match
         racket/port
         racket/string
         "private/date.rkt")

(provide
 (contract-out
  [schedule? (-> any/c boolean?)]
  [parse-schedule (->* (string?) (boolean?) schedule?)]
  [schedule-next (->* (schedule?) (exact-integer?) exact-integer?)]
  [schedule->string (-> schedule? string?)]))

(struct schedule (local? seconds minutes hours days months week-days)
  #:transparent
  #:property prop:evt (lambda (s)
                        (define timestamp
                          (schedule-next s))
                        (handle-evt
                         (alarm-evt (* timestamp 1000))
                         (λ (_) (values s timestamp)))))

(define (make-schedule local? seconds minutes hours days months week-days)
  (let ([seconds   (reduce-field 'seconds   0 59 seconds)]
        [minutes   (reduce-field 'minutes   0 59 minutes)]
        [hours     (reduce-field 'hours     0 23 hours)]
        [days      (reduce-field 'days      1 31 days)]
        [months    (reduce-field 'months    1 12 months)]
        [week-days (reduce-field 'week-days 1 7  week-days)])
    (unless (or (eq? days '*)
                (eq? months '*)
                (pair? week-days)
                (dates-satisfiable? months days))
      (error 'schedule "unsatisfiable day & month constraints~n days: ~a~n months: ~a"
             (~field days)
             (~field months)))
    (schedule local? seconds minutes hours days months week-days)))

(define (schedule-next s [timestamp (current-seconds)])
  (let/ec esc
    (match-define (schedule local? seconds minutes hours days months week-days) s)
    #;(println (date->string (seconds->date timestamp local?) #t))
    (match-define (date second minute hour day month year week-day- _ _ _)
      (seconds->date timestamp local?))
    (unless (or (eq? days '*)
                (eq? week-days '*))
      (define w-timestamp (schedule-next (struct-copy schedule s [days '*]) timestamp))
      (define d-timestamp (if (dates-satisfiable? months days)
                              (schedule-next (struct-copy schedule s [week-days '*]) timestamp)
                              +inf.0))
      (esc (if (< w-timestamp d-timestamp) w-timestamp d-timestamp)))

    (define next-second (next seconds second))
    (unless (= next-second second)
      (define-values (next-year next-month next-day next-hour next-minute)
        (if (< next-second second)
            (+minute year month day hour minute)
            (values year month day hour minute)))
      (esc (schedule-next s (find-seconds next-second next-minute next-hour next-day next-month next-year local?))))

    (define next-minute (next minutes minute))
    (unless (= next-minute minute)
      (define-values (next-year next-month next-day next-hour)
        (if (< next-minute minute)
            (+hour year month day hour)
            (values year month day hour)))
      (esc (schedule-next s (find-seconds 0 next-minute next-hour next-day next-month next-year local?))))

    (define next-hour (next hours hour))
    (unless (= next-hour hour)
      (define-values (next-year next-month next-day)
        (if (< next-hour hour)
            (+day year month day)
            (values year month day)))
      (esc (schedule-next s (find-seconds 0 0 next-hour next-day next-month next-year local?))))

    (define next-month (next months month))
    (unless (= next-month month)
      (define next-year
        (if (< next-month month) (add1 year) year))
      (esc (schedule-next s (find-seconds 0 0 0 1 next-month next-year local?))))

    (define next-day (next days day))
    (define month-durations (get-month-durations year))
    (define month-duration (list-ref month-durations (sub1 month)))
    (unless (and (=  next-day day)
                 (<= next-day month-duration))
      (define-values (next-year next-month)
        (if (or (< next-day day)
                (> next-day month-duration))
            (+month year month)
            (values year month)))
      (esc (schedule-next s (find-seconds 0 0 0 next-day next-month next-year local?))))

    (define week-day (if (= week-day- 0) 7 week-day-))
    (define next-week-day (next week-days week-day))
    (unless (= next-week-day week-day)
      (define-values (next-year next-month next-day)
        (+days year month day (modulo (- next-week-day week-day) 7)))
      (esc (schedule-next s (find-seconds 0 0 0 next-day next-month next-year local?))))

    timestamp))

(define (schedule->string s)
  (match-define (schedule _ seconds minutes hours days months week-days) s)
  (string-join (map ~field `(,seconds ,minutes ,hours ,days ,months ,week-days)) " "))

(define (dates-satisfiable? months days)
  (for*/or ([month (in-list months)]
            [duration (in-list (get-month-durations* month))]
            [day (in-list days)])
    (<= day duration)))

(define (next cs n)
  (cond
    [(eq? cs '*) n]
    [(for/first ([m (in-list cs)] #:when (>= m n)) m)]
    [else (car cs)]))

(define (~field v)
  (if (eq? v '*) "*" (string-join (map number->string v) ",")))


;; validation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reduce-field who lo hi cs)
  (define (in-range? n)
    (and (>= n lo)
         (<= n hi)))
  (let loop ([cs cs]
             [rs null])
    (match cs
      ['()
       (when (null? rs)
         (error 'schedule "field ~a is not satisfiable" who))
       (sort (remove-duplicates rs) <)]

      [`(* . ,_) '*]

      [`((range * * ,step) . ,rst)
       (loop rst (append (build-range lo hi step) rs))]

      [`((range ,n ,m ,step) . ,rst)
       (unless (and (in-range? n)
                    (in-range? m))
         (error 'schedule "expected values between ~a and ~a but got ~a-~a~n field: ~a" lo hi n m who))
       (loop rst (append (build-range n m step) rs))]

      [`((range ,n ,m) . ,rst)
       (unless (and (in-range? n)
                    (in-range? m))
         (error 'schedule "expected values between ~a and ~a but got ~a-~a~n field: ~a" lo hi n m who))
       (loop rst (append (build-range n m) rs))]

      [`(,n . ,rst)
       (unless (in-range? n)
         (error 'schedule "expected a value between ~a and ~a but got ~a~n field: ~a" lo hi n who))
       (loop rst (cons n rs))])))

(define (build-range lo hi [step 1])
  (for/list ([n (in-inclusive-range lo hi step)]) n))


;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-schedule s [local? #t])
  (call-with-input-string s (λ (in) (read-schedule in local?))))

(define (read-schedule [in (current-input-port)] [local? #t])
  (port-count-lines! in)
  (match (read-fields in)
    [`(,minutes ,hours ,days ,months ,week-days)
     (make-schedule local? '(0) minutes hours days months week-days)]

    [`(,seconds ,minutes ,hours ,days ,months ,week-days)
     (make-schedule local? seconds minutes hours days months week-days)]

    [fs
     (error 'read-schedule "expected 5 or 6 fields but got ~a~n fields: ~e" (length fs) fs)]))

(define (skip-whitespace in)
  (match (peek-char in)
    [(or #\space #\tab)
     (skip-char in)
     (skip-whitespace in)]
    [_ (void)]))

(define (skip-char in)
  (void (read-char in)))

(define (syntax-error in fmt . args)
  (define-values (line col pos)
    (port-next-location in))
  (define pos-str
    (if (and line col)
        (format " line: ~a~n col: ~a" line col)
        (format " pos: ~a" pos)))
  (error 'parse-field (format "syntax error: ~a~n~a" (apply format fmt args) pos-str)))

(define (read-fields in)
  (let loop ([fields null])
    (skip-whitespace in)
    (if (eof-object? (peek-char in))
        (reverse fields)
        (loop (cons (read-field in) fields)))))

(define (read-field in)
  (let loop ([rs null])
    (define r
      (match (peek-char in)
        [#\*
         (read-wildcard-or-range in)]
        [(digit)
         (read-integer-or-range in)]
        [(? eof-object?)
         (syntax-error in "unexpected end of file")]
        [chr
         (syntax-error in "unexpected character '~a'" chr)]))
    (cond
      [(equal? (peek-char in) #\,)
       (skip-char in)
       (loop (cons r rs))]
      [else
       (reverse (cons r rs))])))

(define (read-wildcard-or-range in)
  (skip-char in)
  (cond
    [(equal? (peek-char in) #\/)
     (skip-char in)
     `(range * * ,(read-integer in))]
    [else
     '*]))

(define (read-integer-or-range in)
  (define lo
    (read-integer in))
  (cond
    [(equal? (peek-char in) #\-)
     (skip-char in)
     (define hi
       (read-integer in))
     (unless (>= hi lo)
       (syntax-error in "invalid range ~a-~a" lo hi))
     (cond
       [(equal? (peek-char in) #\/)
        (skip-char in)
        `(range ,lo ,hi ,(read-integer in))]
       [else
        `(range ,lo ,hi)])]
    [else
     lo]))

(define (read-integer in)
  (cond
    [(char=? (peek-char in) #\0)
     (begin0 0
       (skip-char in))]
    [else
     (let loop ([n 0])
       (match (peek-char in)
         [(digit d)
          (skip-char in)
          (loop (+ (* n 10) d))]
         [_
          n]))]))

(define (char->digit c)
  (- (char->integer c) 48))

(define-match-expander digit
  (lambda (stx)
    (syntax-parse stx
      [(_) #'(or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)]
      [(_ v) #'(and (digit) (app char->digit v))])))
