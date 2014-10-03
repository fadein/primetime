(use posix srfi-18 srfi-19)

(define factor-time (foreign-lambda void "factor_time" unsigned-integer32))

(let ((start (time-second (current-time))))
  (print "start=" start)
  (let loop ((now start))
	(factor-time now)
	(let ((then (seconds->time (add1 now))))
	  (print "then=" then)
	  (thread-sleep! then)
	  (loop then))))

; a clock with no drift:
;(let ((start (time->seconds (current-time)))
;	  (let loop ((x 1))
;		(thread-sleep! (seconds->time (+ x start)))
;		(write x)
;		(loop (+ x 1))))
