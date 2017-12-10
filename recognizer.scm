(declare (unit recognizer))

(module recognizer
        (advance-prime-count
          circle-incr 
          make-prime-counter
          recognizer )

(import scheme chicken srfi-1)

; this syntax isn't exported, nor seems to be imported into primetime
; something about units vs. syntax?
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

	  ((list= eq? (drop lst 2) '(8 0))
	   'octomus)

	  ((list= eq? (drop lst 2) '(6 0))
	   'sexy)

	  ((list= eq? (drop lst 2) '(4 0))
	   'cousin)

	  ((list= eq? (drop lst 2) '(2 0))
	   'twin)

	  ; 59 is the 17th prime - this also marks a full
	  ; minute without a prime number occurring
	  ((> (list-ref lst 2) 59)
	   'combo-breaker)

	  (else
		#f))))

); module
