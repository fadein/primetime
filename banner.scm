; vim:set tw=64 expandtab:

(use
  ansi-escape-sequences)

(define bannerText
  (string-split
  #<<BANNER
## *** ***** ******* *********** ************* *****************
** *** ***** ******* *  IT'S PRIME TIME!  **** *****************
** *** ***** ******* *   By Erik Falor    **** *****************
** *** ***** ******* * Copyright (C) 2014 **** *****************
## *** ***** ******* *********** ************* *****************
BANNER
"\n"
))

(define (banner)
  (apply print
         (map (lambda (s) (set-text '(bold bg-red fg-white) (conc s "\r\n")))
              bannerText)))
