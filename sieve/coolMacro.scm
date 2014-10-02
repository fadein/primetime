;(define-syntax condo
;   (er-macro-transformer
;	 (lambda (exp rename compare)
;	   (let ((clauses (cdr exp)))
;		 (if (null? clauses)
;		   `(,(rename 'quote) unspecified)
;		   (let* ((first (car clauses))
;				  (rest (cdr clauses))
;				  (test (car first)))
;			 (cond ((and (symbol? test)
;						 (compare test (rename 'else)))
;					`(,(rename 'begin) ,@(cdr first)))
;				   (else `(,(rename 'if)
;							,test
;							(,(rename 'begin) ,@(cdr first))
;							(,(rename 'condo) ,@rest))))))))))

(define-syntax prime-directive
  (er-macro-transformer
	(lambda (expr rename compare)
	  (let* ((num (cadr expr))
			 (less (- num 1)))
		(print expr " " num)
		(if (zero? less)
		  `(,(rename 'cons) ,num '())
		  `(,(rename 'cons) ,num (,(rename 'prime-directive) ,less)))))))

(define-syntax prime-directive2
  (er-macro-transformer
	(lambda (expr rename compare)
	  (let* ((num (cadr expr))
			 (less (- num 1)))
		(print expr " " num " " less)
		(if (zero? less)
		  (cons num less)
		  (cons num (prime-directive2 ,less)))))))

(define-syntax prime-directive3
  (ir-macro-transformer
	(lambda (expr inject compare)
	  (let* ((num (cadr expr))
			 (less (- num 1)))
		(print expr " " num " " less)
		(if (zero? less)
		  `(begin (cons ,num ,less))
		  `(begin (cons ,num (prime-directive3 ,less))))))))

; pretty close - but generates lots of calls to C_a_i_cons() in the C code
(define-syntax prime-directive4
  (ir-macro-transformer
	(lambda (expr inject compare)
	  (let* ((num (cadr expr))
			 (less (- num 1)))
		(if (zero? less)
		  `(cons ,num ,less)
		  `(cons (* ,num ,num) (prime-directive4 ,less)))))))

; better - doesn't leave a . 0 at the end of the list
(define-syntax prime-directive5
  (ir-macro-transformer
	(lambda (expr inject compare)
	  (let* ((num (cadr expr))
			 (less (- num 1)))
		(if (zero? less)
		  `(cons ,num '())
		  `(cons (* ,num ,num) (prime-directive5 ,less)))))))


(define-syntax prime-directive6
  (ir-macro-transformer
	(lambda (expr inject compare)
	  (let ((num (cadr expr)))
		  `(begin ((lambda (N) 
(let* ((max-index (max 0 (quotient (- N 3) 2)))
		 (v (make-vector (+ 1 max-index) #t)))
	(let loop ((i 0)) ; i is the current index on the tape
	  (cond
		((vector-ref v i)
		 (let* ((prime (+ i i 3)) ; newly found prime
				(p2i (+ i (* (+ i 1) prime)))) ; the index of prime^2
		   (do ((j p2i (+ j prime)))
			 ((> j max-index))
			 (vector-set! v j #f))
		   (if (> p2i max-index) ; no point to continue elimination, so
			 ; we collect the primes, scanning backwards
			 (let collect ((i max-index) (primes '()))
			   (cond
				 ((negative? i) (cons 2 primes))
				 ((vector-ref v i) (collect (- i 1) (cons (+ i i 3) primes)))
				 (else (collect (- i 1) primes))))
			 (loop (+ 1 i)))))
		(else (loop (+ 1 i))))))

					 ) ,num) )))))

(print 'prime-directive6 " " (prime-directive6 10000000))
(print 'prime-directive5 " " (prime-directive5 10))
;(print (prime-directive4 10))
;(print '(100 81 64 49 36 25 16 9 4 1))
