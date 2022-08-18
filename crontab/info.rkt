#lang info

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
  '(("crontab-manual.scrbl")))
