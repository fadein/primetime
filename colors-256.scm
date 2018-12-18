(declare (unit colors-256))
(module colors-256
        (keyframe
          keyframes
          special-colors
          prime-colors)

(import
  ansi-escape-sequences
  (chicken base)
  scheme
  srfi-1
  )


(define (keyframe i frames)
  (define (proportional+ p rgb1 rgb2)
	(list (round (+ (* (- 1 p) (list-ref rgb1 0)) (* p (list-ref rgb2 0))))
		  (round (+ (* (- 1 p) (list-ref rgb1 1)) (* p (list-ref rgb2 1))))
		  (round (+ (* (- 1 p) (list-ref rgb1 2)) (* p (list-ref rgb2 2))))))
  ; locate the segment in which i lies
  (cond
	((<= i (caar keyframes))
	 (cadar keyframes))
	((> i (car (last keyframes)))
	 (cadr (last keyframes)))
	(else
	  (let-values (((low high)
					(let loop ((head (car keyframes)) (tail (cdr keyframes)))
					  (if (<= i (caar tail))
						(values head (car tail))
						(loop (car tail) (cdr tail))))))
				  ;(print "low:"  low " high:" high)
				  (let* ((proportion (/ (- i (car low)) (- (car high) (car low)))))
					(proportional+ proportion (cadr low) (cadr high)))))))

; this list should be sorted on keyframe index
(define keyframes
  '((20  (#xff #x00 #xff))  ;magenta
	(30  (#xff #x00 #x00))  ;red
	(40  (#xff #xff #x00))  ;yellow
	(50  (#x00 #xff #x00))  ;green
	(60  (#x00 #xff #xff))  ;cyan
	(70  (#x00 #x00 #xff))  ;blue
	(90  (#xb0 #xb0 #xb0))  ;pale grey
	(101 (#x50 #x50 #x50))));dim grey

; re-define some of the colors in the terminal's 6x6x6 color cube
(do ((i 20 (add1 i)))
  ((= i 102))
  (let ((color (keyframe i keyframes)))
	(print* (set-color256! i
						   (first  color)
						   (second color)
						   (third  color)))))

;; Prepare the color cycle for output
(define special-colors 
  ; form a list of the extended 256 color codes
  (let loop ((i 20))
	(if (= i 101)
	  (circular-list `(bold (foreground ,i)))
	  (cons `(bold (foreground ,i)) (loop (add1 i))))))

(define prime-colors (drop special-colors 10))

); module
