#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/match
         "schedule.rkt")

(provide
 current-crontab-schedule
 crontab)

(define-logger crontab)

(define current-crontab-schedule
  (make-parameter #f))

(define-syntax (crontab stx)
  (syntax-parse stx
    [(_ [schedule:expr proc:expr] ...+)
     #'(make-crontab `((,schedule . ,proc) ...))]))

(define (make-crontab table) ;; noqa
  (define cust (make-custodian))
  (define thds
    (parameterize ([current-custodian cust])
      (for/list ([entry (in-list table)])
        (match-define (cons schedule-str proc) entry)
        (define schedule (parse-schedule schedule-str))
        (define proc-name (object-name proc))
        (thread
         (procedure-rename
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
                      (define t0 (current-inexact-monotonic-milliseconds))
                      (define deadline (alarm-evt (+ (* timestamp 1000) 1000)))
                      (log-crontab-debug "executing cron procedure ~s" proc-name)
                      (with-handlers ([exn:fail? (λ (e) ((error-display-handler) (exn-message e) e))])
                        (parameterize ([current-crontab-schedule schedule])
                          (proc timestamp)))
                      (define dt (- (current-inexact-monotonic-milliseconds) t0))
                      (log-crontab-debug "cron procedure ~s completed after ~sms" proc-name dt)
                      (loop deadline)))))))
          (string->symbol
           (format "crontab:~a" proc-name)))))))
  (lambda ()
    (log-crontab-debug "stopping cron threads")
    (for ([t (in-list thds)])
      (thread-send t '(stop)))
    (log-crontab-debug "waiting for threads to stop")
    (for-each thread-wait thds)
    (log-crontab-debug "threads stopped")
    (custodian-shutdown-all cust)))
