(use posix)

(define factor-time (foreign-lambda void "factor_time" unsigned-integer32))

;(let forever ((now (current-seconds)))
;  (print "now='" now "'")
;  (factor-time now)
;  (sleep 1)
;  (newline)
;  (forever (add1 now)))

(let ((now 1412200537))
  (print "now is " now)
  (factor-time now))
(print "  Should output 11 128381867")

