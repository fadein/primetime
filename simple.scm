(use srfi-18)

(let ((start (time->seconds (current-time))))
  (let loop ((x 1))
	(let ((then (+ x start)))
	  (thread-sleep! (seconds->time then))
	  (print then)
	  (loop (+ x 1)))))
