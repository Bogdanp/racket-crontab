#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         "schedule.rkt")

(provide
 crontab)

(define-syntax (crontab stx)
  (syntax-parse stx
    [(_ [schedule:expr proc:expr] ...+)
     #'(make-crontab `((,schedule . ,proc) ...))]))

(define (make-crontab table)
  (define cust (make-custodian))
  (define thds
    (parameterize ([current-custodian cust])
      (for/list ([entry (in-list table)])
        (define schedule (parse-schedule (car entry)))
        (define proc (cdr entry))
        (thread
         (lambda ()
           (let loop ([deadline-evt #f])
             (sync
              (handle-evt
               (thread-receive-evt)
               void)
              (if deadline-evt
                  (handle-evt
                   deadline-evt
                   (lambda (_)
                     (loop #f)))
                  (handle-evt
                   schedule
                   (lambda (_ timestamp)
                     (define deadline (alarm-evt (+ (* timestamp 1000) 1000)))
                     (with-handlers ([exn:fail? (λ (e) ((error-display-handler) (exn-message e) e))])
                       (proc timestamp))
                     (loop deadline)))))))))))
  (lambda ()
    (for ([t (in-list thds)])
      (thread-send t '(stop)))
    (for-each thread-wait thds)
    (custodian-shutdown-all cust)))
