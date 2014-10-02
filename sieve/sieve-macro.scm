(define-syntax prime-sieve-macro
  (ir-macro-transformer
	(lambda (expr inject compare)
	  (let ((num (cadr expr)))
		`(list ,@((lambda (N) 
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
						 (else (loop (+ 1 i)))))))
				 num))))))


(time (print (length (prime-sieve-macro 50000))))
