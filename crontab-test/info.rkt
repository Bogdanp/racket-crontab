#lang info

(define collection "tests")
(define deps
  '("base"
    "crontab-lib"))
(define build-deps
  '("base"
    "crontab-lib"
    "rackunit-lib"))
(define implies
  '("crontab-lib"))
