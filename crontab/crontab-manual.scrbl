#lang scribble/manual

@(require scribble/example
          (for-label crontab
                     racket/base
                     racket/contract
                     racket/date))

@title{@tt{crontab}: @tt{cron}-like scheduling}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[crontab]

This package provides functionality for scheduling work inside of
Racket using @tt{cron}-like syntax.

@section{Reference}

@(define (reftech text)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") text))

@defform[
  (crontab
   [schedule-expr handler-expr] ...+)
  #:contracts ([schedule-expr string?]
               [handler-expr (-> exact-integer? any)])
]{
  Spawns a thread for every entry in the table, where each thread
  synchronizes on the schedule and executes its associated handler
  procedure when the schedule is ready for synchronization.  Returns a
  procedure that stops all the threads when applied.

  Handlers are executed synchronously within each thread, meaning
  procedures that take a long time to execute may cause runs to be
  skipped.

  @examples[
    (require crontab)
    (define stop
     (crontab
      ["* * * * * *" println]))
    (sleep 5)
    (stop)
  ]
}

@deftogether[(
  @defproc[(schedule? [v any/c]) boolean?]
  @defproc[(parse-schedule [s string?]
                           [local-time? boolean? #t]) schedule?]
)]{
  Parses the cron schedule specification represented by @racket[s] and
  returns a schedule.  Supports the same syntax as BSD cron, with the
  following differences:

  @itemlist[
    @item{``@"@"'' commands are not supported}
    @item{``0'' is not a valid weekday}
    @item{month and weekday names are not supported}
  ]

  When a schedule includes 6 fields, the first field is treated as a
  constraint on the seconds component of a timestamp.

  Timestamps are processed by the schedule according to the local time
  zone if @racket[local-time?] is @racket[#t] and UTC otherwise.

  Schedule values are @reftech{synchronizable events}.  A schedule is
  ready for synchronization when the value of
  @racket[(current-seconds)] is less than or equal to the value of
  @racket[(schedule-next s)] as of the moment the event is
  synchronized on.  The @reftech{synchronization result} of a schedule
  is the schedule itself and the timestamp it became ready for
  synchronization at.

  @examples[
    (require crontab racket/date)
    (define (~date ts)
     (parameterize ([date-display-format 'iso-8601])
      (date->string (seconds->date ts) #t)))

    (code:line)
    (define now
     (find-seconds 30 21 10 19 8 2022))
    (code:line)

    (~date now)
    (~date (schedule-next (parse-schedule "* * * * * *") now))
    (~date (schedule-next (parse-schedule "* * * * *") now))
    (~date (schedule-next (parse-schedule "0 5-23/5 * * *") now))
    (~date (schedule-next (parse-schedule "0 12 * * 5") now))
    (~date (schedule-next (parse-schedule "* * 29 2 *") now))

    (code:line)
    (let ([s (parse-schedule "0 * * * *")])
     (for/fold ([ts now])
               ([_ (in-range 10)])
      (define new-ts
       (schedule-next s ts))
      (begin0 (add1 new-ts)
       (displayln (~date new-ts)))))

    (code:line)
    (sync (parse-schedule "* * * * * *"))
  ]
}

@defproc[(schedule-next [s schedule?] [start-timestamp exact-integer? (current-seconds)]) exact-integer?]{
  Returns a timestamp representing the first moment that matches the
  schedule including and after @racket[start-timestamp].
}

@defproc[(schedule->string [s schedule?]) string?]{
  Returns a representation of the schedule in cron format.
}
