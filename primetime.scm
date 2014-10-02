(use posix)

(define factor-time (foreign-lambda void "factor_time" unsigned-integer32))

(let forever ((now (current-seconds)))
  (factor-time now)
  (sleep 1)
  (forever (add1 now)))

