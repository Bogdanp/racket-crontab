#lang info

(define license 'BSD-3-Clause)
(define collection "tests")
(define deps
  '("base"
    "crontab-lib"))
(define build-deps
  '("base"
    "crontab-lib"
    "rackcheck-lib"
    "rackunit-lib"))
(define implies
  '("crontab-lib"))
