#lang info

(define license 'BSD-3-Clause)
(define collection "crontab")
(define deps
  '("base"
    "crontab-lib"))
(define build-deps
  '("base"
    "racket-doc"
    "scribble-lib"))
(define implies
  '("crontab-lib"))
(define scribblings
  '(("crontab-manual.scrbl" () ("Web Development"))))
