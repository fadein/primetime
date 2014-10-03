(use posix srfi-18 )

(define factor-time (foreign-lambda void "factor_time" unsigned-integer32))

(let ((start (time->seconds (current-time))))
  (let loop ((x 1)
			 (now (current-seconds)))
	(factor-time now)
	(thread-sleep! (seconds->time (+ x start)))
	(loop (+ 1 x) (+ 1 now))))
