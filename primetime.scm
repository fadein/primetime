(use posix srfi-4 srfi-18)

(define factor-time (foreign-lambda void "factor_time"
									unsigned-integer32
									u32vector
									integer))

(define *MAX-SIZE* 30)

(define factors (make-u32vector *MAX-SIZE*))

(let ((start (time->seconds (current-time))))
  (let loop ((x 1)
			 (now (current-seconds)))
	(factor-time now factors *MAX-SIZE*)
	;(printf "~a: ~a~n" now (subu32vector factors 1 (u32vector-ref factors 0)))
	(printf "~a: ~a~n~n" now factors )
	(thread-sleep! (seconds->time (+ x start)))
	(loop (+ 1 x) (+ 1 now))))
