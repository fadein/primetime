(use posix srfi-4 srfi-18 ansi-escape-sequences)

;; DEBUGGING
;(set! current-seconds (lambda () 1412316039.0))

(define factor-time (foreign-lambda void "factor_time"
									unsigned-integer32
									u32vector
									integer))

(define *MAX-SIZE* 30)

(define colors
  (concatenate
	(list
	  (make-list 2 '(bold fg-magenta))
	  (make-list 2 '(bold fg-red))
	  (make-list 3 '(bold fg-white))
	  (make-list 4 '(bold fg-yellow))
	  (make-list 5 '(bold fg-green))
	  (make-list 6 '(bold fg-cyan))
	  (make-list 7 '(bold fg-blue)))))

(set-cdr! (last-pair colors) (circular-list '(bold fg-black)))

(define beginner (cdddr colors))

(define u32factors (make-u32vector *MAX-SIZE*))

(let ((start (time->seconds (current-time)))
	  (now (inexact->exact (current-seconds))))
  (let loop ((x 1) (now now) (prev-prime 1000) (c beginner))

	(let-syntax ((doloop
				   (syntax-rules ()
								 ((_ cc tt pp)
								  (begin
									(print (set-text (car cc) tt))
									(thread-sleep! (seconds->time (+ x start)))
									(loop (+ 1 x) (+ 1 now) pp cc))))))

	  ; call the C function and put the list of factors into u32factors
	  (factor-time now u32factors *MAX-SIZE*)

	  ; the 1st elemet of u32factors is the count of prime factors
	  (let* ((n (u32vector-ref u32factors 0))
			 (prime? (= 1 n)))

		(cond ((and prime? (= prev-prime 1))
			   (doloop
				 colors
				 (conc now ": ******************** TWIN PRIME!!! ********************")
				 0))

			  (prime?
				(doloop
				  (cddr colors)
				  (conc now ": ********** PRIME TIME! **********")
				  0))

			  (else
				(let ((factors (subu32vector u32factors 1 (+ 1 n))))
				  (doloop
					(cdr c)
					(conc now ": " (u32vector->list factors))
					(+ 1 prev-prime)))))))))
