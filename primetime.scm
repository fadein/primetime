(use
  ansi-escape-sequences
  data-structures
  posix
  srfi-13
  srfi-18
  srfi-4)

(include "recognizer.scm")

;; DEBUGGING
;(set! current-seconds (lambda () 99131.0)) ;; Quadruple
;(set! current-seconds (lambda () 1412316039.0)) ;; TRIPLET
;(set! current-seconds (lambda () 1412360565.0)) ;; TWIN PRIMES
;(set! current-seconds (lambda () 1412360693.0)) ;; immediate prime, then 60 gap
;(set! current-seconds (lambda () 1415577600.0)) ;; Error: (subu32vector) out of range

;; Action to take upon process exit - show the cursor and reset the terminal's color
(define (cleanup signal)
  (print (show-cursor) (set-text '(fg-white) ""))
  (exit))
(set-signal-handler! signal/term  cleanup)
(set-signal-handler! signal/int   cleanup)
(set-signal-handler! signal/pipe  cleanup)
(set-signal-handler! signal/quit  cleanup)

;; make ready for the factor_time C function
(define factor-time (foreign-lambda void "factor_time"
									unsigned-integer32
									u32vector
									integer))

(define *MAX-FACTORS* 32)

;; an array of unsigned ints to write prime factors into as a side-effect
(define u32factors (make-u32vector *MAX-FACTORS* 0))

;; either use the current epoch time or the command-line argument
(define (what-time?)
  (if (and (not (null? (command-line-arguments)))
		   (> (string-length (car (command-line-arguments))) 9)
		   (string->number (car (command-line-arguments))))
	(string->number (car (command-line-arguments)))
	(current-seconds)))

;;; main code
; we don't need no stinkin' input
(close-input-port (current-input-port))

; Set up fancy colors and print the title banner
(include "256colors.scm")
(print* (hide-cursor) (set-title "IT'S PRIME TIME!!!") (erase-display) (cursor-position))
(include "banner.scm")
(print* (banner (drop special-colors 5)))

(let ((start (time->seconds (current-time)))
	  (now (what-time?))
	  (prime-counter (make-prime-counter)))
  (let loop ((x 1) (now now) (prev-prime 1000) (c (drop special-colors 9)))

	(let-syntax ((doloop
				   (syntax-rules ()
								 ((_ cc tt)
								  (begin
									;(print "\n\tthe prime list was " (take prime-counter 4))
									(circle-incr prime-counter)
									;(print "\n\tthe prime list is now " (take prime-counter 4))

									(print* "\n" (set-text256 (car cc) tt ))
									(thread-sleep! (seconds->time (+ x start)))
									(loop (+ 1 x) (+ 1 now) 0 cc)))

								 ((_ cc tt pp)
								  (begin
									;(print "\n\tthe prime list was " (take prime-counter 4))
									(circle-incr prime-counter)
									;(print "\n\tthe prime list is now " (take prime-counter 4))

									(print* "\n" (set-text256 (car cc) tt ))
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
								": ** *** PRIME ******* QUADRUPLET! ************* *****************\r")))

			  ((triplet)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** PRIME TRIPLET *********** ************* *****************\r")))

			  ((octomus)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** ***** OCTOMUS ***PRIME***\r")))

			  ((sexy)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** *SEXY PRIME**\r")))

			  ((cousin)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** PRIME COUSIN* ***********\r")))

			  ((twin)
			   (doloop
				 special-colors
				 (string-append now-str
								": ** *** *TWIN PRIME** *********** *************\r")))

			  ((combo-breaker)
				 (doloop
				   prime-colors
				   (string-append now-str
								": ** CCC COMBO BREAKER *PRIME*GAP="
								(number->string (- (list-ref prime-counter 2) 1)) "\r")))

			  (else
				(doloop
				  prime-colors
				  (string-append now-str
								 ": ** *** PRIME TIME***\r")))))

				(let ((factors (subu32vector u32factors 1 (+ 1 n))))
				  (doloop
					(cdr c)
					(string-append now-str ": " (string-join (map number->string (u32vector->list factors))) "\r")
					(+ 1 prev-prime))))))))
