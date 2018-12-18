; vim: set expandtab:

(declare (unit banner))

;; Print a colorful banner
(module banner
        (banner)

(import
  ansi-escape-sequences
  (chicken string)
  scheme
  srfi-13)

(define bannerText
  (string-split
  #<<BANNER
## *** ***** ******* *********** ************* *****************
** *** ***** ******* *  IT'S PRIME TIME!  **** *****************
** *** ***** ******* *   By Erik Falor    **** *****************
** *** *****  https://github.com/fadein/primetime **************
** *** ***** ******* * Copyright (c) 2018 **** *****************
** *** ***** ******* *********** ************* #################
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
