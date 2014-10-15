; vim:set tw=64 expandtab:

;; Print a colorful banner

(use ansi-escape-sequences srfi-13)

(define bannerText
  (string-split
  #<<BANNER
## *** ***** ******* *********** ************* *****************
** *** ***** ******* *  IT'S PRIME TIME!  **** *****************
** *** ***** ******* *   By Erik Falor    **** *****************
** *** ***** ******* * Copyright (C) 2014 **** *****************
** ### ***** ******* *********** ************* *****************
BANNER
    "\n"
))

(define (banner attrb)
  (string-chomp
    (string-join
      (map (lambda (str attrb) (set-text256 attrb str #t))
           bannerText attrb)
      "\r\n") "\r\n"))
