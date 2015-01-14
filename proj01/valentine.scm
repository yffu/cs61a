;P6

(define (valentine c d)
  (cond ((heart? c) ((stop-at 19) c d))
	(else ((stop-at 17) c d))))

(define (heart? c)
  (cond ((empty? c) #f)
	((member? 'h (last c)) #t)
	(else (heart? (bl c)))))
