(use posix srfi-4 srfi-18)

(define factor-time (foreign-lambda void "factor_time"
									unsigned-integer32
									u32vector
									integer))

(define *MAX-SIZE* 30)

(define u32factors (make-u32vector *MAX-SIZE*))

(let ((start (time->seconds (current-time)))
	  (now (inexact->exact (current-seconds))))
  (let loop ((x 1) (now now) (prev-prime 1000))

	(let-syntax ((doloop
				   (syntax-rules ()
								 ((_ pp)
								  (begin
									(thread-sleep! (seconds->time (+ x start)))
									(loop (+ 1 x) (+ 1 now) 0))))))

	  ; call the C function and put the list of factors into u32factors
	  (factor-time now u32factors *MAX-SIZE*)

	  ; the 1st elemet of u32factors is the count of prime factors
	  (let* ((n (u32vector-ref u32factors 0))
			 (prime? (= 1 n)))

		(cond ((and prime? (= prev-prime 1))
			   (printf "~a: TWIN PRIME!!!~n" now)
			   (doloop 0))

			  (prime?
				(printf "~a: PRIME TIME!!!~n" now)
				(doloop 0))

			  (else
				(let ((factors (subu32vector u32factors 1 (+ 1 n))))
				  (print now ": " (u32vector->list factors))
				  (doloop (+ 1 prev-prime)))))))))
