(use
  data-structures
  srfi-4
  srfi-13
  srfi-18
  ansi-escape-sequences)

(include "recognizer.scm")

;; DEBUGGING
;(set! current-seconds (lambda () 99131.0)) ;; Quadruple
;(set! current-seconds (lambda () 1412316039.0)) ;; TRIPLET
;(set! current-seconds (lambda () 1412360565.0)) ;; TWIN PRIMES
;(set! current-seconds (lambda () 1412360693.0)) ;; immediate prime, then 60 gap

;; make ready for the factor_time C function
(define factor-time (foreign-lambda void "factor_time"
									unsigned-integer32
									u32vector
									integer))

(define *MAX-FACTORS* 26)

;; an array of unsigned ints to write prime factors into as a side-effect
(define u32factors (make-u32vector *MAX-FACTORS*))

(include "256colors.scm")

;;; main code
; we don't need no stinkin' input
(close-input-port (current-input-port))

; print startup banner
(include "banner.scm")
(banner)

(let ((start (time->seconds (current-time)))
	  (now (current-seconds))
	  (prime-counter (make-prime-counter)))
  (let loop ((x 1) (now now) (prev-prime 1000) (c (cdr prime-colors)))

	(let-syntax ((doloop
				   (syntax-rules ()
								 ((_ cc tt pp)
								  (begin
									;(print "\n\tthe prime list was " (take prime-counter 4))
									(circle-incr prime-counter)
									;(print "\n\tthe prime list is now " (take prime-counter 4))

									(print* "\n" (set-text256 (car cc) tt))
									(thread-sleep! (seconds->time (+ x start)))
									(loop (+ 1 x) (+ 1 now) pp cc))))))

	  ; call the C function and put the list of factors into u32factors
	  (factor-time now u32factors *MAX-FACTORS*)

	  ; the 1st element of u32factors is the count of prime factors
	  (let* ((n (u32vector-ref u32factors 0))
			 (prime? (= 1 n))
			 (now-str (substring (number->string now) 0 10)))

		(if prime?
		  (begin
			(advance-prime-count prime-counter)
			;(print "\nrecognizing on " (take prime-counter 4)
				   ;(recognizer prime-counter))

			(case (recognizer prime-counter)
			  ((quadruple)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** PRIME ******* QUADRUPLET! ************* *****************\r")
				 0))

			  ((triplet)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** PRIME TRIPLET *********** ************* *****************\r")
				 0))

			  ((sexy)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** *SEXY PRIME**\r")
				 0))

			  ((cousin)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** PRIME COUSIN* ***********\r")
				 0))

			  ((twin)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** *TWIN PRIME** *********** *************\r")
				 0))

			  ((combo-breaker)
				 (doloop
				   prime-colors
				   (string-append now-str
								": ** CCC COMBO BREAKER *PRIME*GAP=" (number->string (list-ref prime-counter 2)) "\r")
				 0))

			  (else
				(doloop
				  prime-colors
				  (string-append now-str
								 ": ** *** PRIME TIME***\r")
				  0))))

				(let ((factors (subu32vector u32factors 1 (+ 1 n))))
				  (doloop
					(cdr c)
					(string-append now-str ": " (string-join (map number->string (u32vector->list factors))) "\r")
					(+ 1 prev-prime))))))))
