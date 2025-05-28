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

@(define (reftech . content)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") content))
@(define ev (make-base-eval))
@examples[
  #:eval ev
  #:hidden
  (require crontab)
]

@subsection{Crontabs}

@defform[
  (crontab
   [schedule-expr handler-expr] ...+)
  #:contracts ([schedule-expr string?]
               [handler-expr (-> exact-integer? any)])
]{
  Spawns a thread for every entry in the table, where each thread
  synchronizes on the @tech{schedule} and executes its associated
  handler procedure when the schedule is ready for synchronization.
  Returns a procedure that stops all the threads when applied.

  Handlers are executed synchronously within each thread, meaning
  procedures that take a long time to execute may cause runs to be
  skipped.

  @examples[
    #:eval ev
    (define stop
     (crontab
      ["* * * * * *" println]))
    (sleep 5)
    (stop)
  ]

  The spawned threads log information about what is currently being
  run to the ``crontab'' topic.

  @history[#:changed "0.2" @elem{Extended logging.}]
}

@subsection{Schedules}

@deftech{Schedules} constrain the components of a timestamp to match
specific moments in time.

Schedules can be used as @reftech{synchronizable events}.  A schedule
is ready for synchronization when the value of @racket[(current-seconds)]
is less than or equal to the value of @racket[(schedule-next s)] as of
the moment the event is synchronized on.  The @reftech{synchronization
result} of a schedule is the schedule itself and the timestamp (in
seconds) it became ready for synchronization at.

@examples[
  #:eval ev
  (require racket/date)
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
    (displayln (~date new-ts))
    (add1 new-ts)))

  (code:line)
  (sync (parse-schedule "* * * * * *"))
]

@defproc[(schedule? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{schedule}.
}

@defproc[(parse-schedule [s string?]
                         [local-time? boolean? #t]) schedule?]{
  Parses the @tt{cron} schedule represented by @racket[s] and returns
  a @tech{schedule}.  Supports the same syntax as
  @hyperlink["https://man.openbsd.org/crontab.5"]{BSD @tt{cron}},
  with the following differences:

  @itemlist[
    @item{``@"@"'' commands are not supported,}
    @item{``0'' is not a valid weekday, and}
    @item{month and weekday names are not supported.}
  ]

  When a schedule contains 6 fields instead of 5, the first field is
  treated as a constraint on the seconds component of a timestamp.

  Timestamps are processed by the schedule according to the local time
  zone if @racket[local-time?] is @racket[#t] and UTC otherwise.
}

@defproc[(schedule-matches? [s schedule?] [timestamp exact-integer?]) boolean?]{
  Returns @racket[#t] when @racket[timestamp] matches the schedule.
}

@defproc[(schedule-next [s schedule?] [start-timestamp exact-integer? (current-seconds)]) exact-integer?]{
  Returns a timestamp representing the first moment that matches the
  schedule including and after @racket[start-timestamp].
}

@defproc[(schedule->string [s schedule?]) string?]{
  Returns a representation of the schedule in cron format.
}
