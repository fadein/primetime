(use srfi-18)

(let loop ((sleepy (time->seconds (current-time)))
		   (now (current-seconds)))
  (print now)
  (thread-sleep! (seconds->time sleepy))
  (loop (+ 1 sleepy) (+ 1 now)))
