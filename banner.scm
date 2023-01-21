; vim: set expandtab:

(declare (unit banner))

;; Print a colorful banner
(module banner
        (banner)

(import
  ansi-escape-sequences
  (chicken time posix)
  (chicken string)
  scheme
  srfi-13)

(define YYY (+ 1900 (vector-ref (seconds->local-time) 5)))

(define bannerText
  (string-split
  #<#BANNER
** @@@ ***** ******* *********** ************* *****************
** *** ***** ******* *  IT'S PRIME TIME!  **** *****************
** *** ***** ******* *   By Erik Falor    **** *****************
** *** *****  https://github.com/fadein/primetime **************
** *** ***** ******* * Copyright (c) #YYY **** *****************
@@ *** ***** ******* *********** ************* *****************
BANNER
    "\n"
))

(define (banner attrb)
  (string-join
    (map (lambda (str attrb)
           (set-text '(reverse-video)
                     (set-text256 attrb str #f))) bannerText attrb)
    "\r\n"))

); module
