(module recognizer
		*
		(import chicken scheme srfi-1)
		(use srfi-1)

		(define-syntax advance-prime-count
		  (syntax-rules ()
						((_ lst)
						 (begin
						   (set-car! lst 0)
						   (set! lst (cdr lst))))))

		;;; Increment each element of a circular list
		(define (circle-incr lst)
		  (do ((i 0 (+ 1 i)))
			((= 4 i))
			(set! (list-ref lst i) (add1 (list-ref lst i)))))

		(define (make-prime-counter)
		  (circular-list -100 -100 -100 -100))

		(define (recognizer lst)
		  (let ((lst (take lst 4)))
			(cond
			  ((list= eq? lst '(8 6 2 0))
			   'quadruple)

			  ((or (list= eq? (drop lst 1) '(6 4 0))
				   (list= eq? (drop lst 1) '(6 2 0)))
			   'triplet)

			  ((list= eq? (drop lst 2) '(6 0))
			   'sexy)

			  ((list= eq? (drop lst 2) '(4 0))
			   'cousin)

			  ((list= eq? (drop lst 2) '(2 0))
			   'twin)

			  ((> (list-ref lst 2) 56)
			   'combo-breaker)

			  (else
				#f)))))
