(use data-structures srfi-4 srfi-13 srfi-18 ansi-escape-sequences)

;; DEBUGGING
;(set! current-seconds (lambda () 1412316039.0))

;; make ready for the factor_time C function
(define factor-time (foreign-lambda void "factor_time"
									unsigned-integer32
									u32vector
									integer))

(define *MAX-FACTORS* 26)

;; an array of unsigned ints to write prime factors into as a side-effect
(define u32factors (make-u32vector *MAX-FACTORS*))

;; Prepare the color cycle for output
(define colors
  (concatenate
	(list
	  (make-list 2 '(bold fg-magenta))
	  (make-list 2 '(bold fg-red))
	  (make-list 3 '(bold fg-white))
	  (make-list 5 '(bold fg-yellow))
	  (make-list 7 '(bold fg-green))
	  (make-list 11 '(bold fg-cyan))
	  (make-list 13 '(bold fg-blue)))))

(set-cdr! (last-pair colors) (circular-list '(bold fg-black)))

;; print startup banner
(print* (set-text '(bold bg-red fg-white)
				  "** *** ***** ******* It's PRIME TIME! *********** *************"))

(let ((start (time->seconds (current-time)))
	  (now (current-seconds)))
  (let loop ((x 1) (now now) (prev-prime 1000) (c (cdddr colors)))

	(let-syntax ((doloop
				   (syntax-rules ()
								 ((_ cc tt pp)
								  (begin
									(print* "\n" (set-text (car cc) tt))
									(thread-sleep! (seconds->time (+ x start)))
									(loop (+ 1 x) (+ 1 now) pp cc))))))

	  ; call the C function and put the list of factors into u32factors
	  (factor-time now u32factors *MAX-FACTORS*)

	  ; the 1st element of u32factors is the count of prime factors
	  (let* ((n (u32vector-ref u32factors 0))
			 (prime? (= 1 n))
			 (now-str (substring (number->string now) 0 10)))

		(cond ((and prime? (= prev-prime 1))
			   (doloop
				 colors
				 (string-append now-str
								": ** *** ***** ******* TWIN PRIME!!! *********** *************")
				 0))

			  (prime?
				(doloop
				  (cddr colors)
				  (string-append now-str
								 ": ** *** ***** ******* PRIME TIME! *********** *************")
				  0))

			  (else
				(let ((factors (subu32vector u32factors 1 (+ 1 n))))
				  (doloop
					(cdr c)
					(string-append now-str ": " (string-join (map number->string (u32vector->list factors))))
					(+ 1 prev-prime)))))))))
