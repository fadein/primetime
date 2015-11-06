; vim:set tw=64 expandtab:

;; Print a colorful banner

(use ansi-escape-sequences srfi-13)

(define bannerText
  (string-split
  #<<BANNER
## *** ***** ******* *********** ************* *****************
** *** ***** ******* *  IT'S PRIME TIME!  **** *****************
** *** ***** ******* *   By Erik Falor    **** *****************
** *** *****  https://github.com/fadein/primetime **************
** *** ***** ******* * Copyright (C) 2015 **** *****************
** *** ***** ####### *********** ************* *****************
BANNER
    "\n"
))

(define (banner attrb)
    (string-join
      (map (lambda (str attrb)
             (set-text '(reverse-video)
                       (set-text256 attrb str #f))) bannerText attrb)
      "\r\n"))
