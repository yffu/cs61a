;P8

(define (majority strategy-1 strategy-2 strategy-3)
  (lambda (c d)
    (cond ((equal? (strategy-1 c d) (strategy-2 c d))
	   (strategy-1 c d))
	  ((equal? (strategy-1 c d) (strategy-3 c d))
	   (strategy-1 c d))
	  (else (strategy-2 c d)))))

(define (majority-strategy c d)
  ((majority stop-at-17 dealer-sensitive valentine) c d))
	  
