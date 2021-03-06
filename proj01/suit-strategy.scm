;P7

(define (suit-strategy suit suit-strat base-strat)
  (lambda (c d)
    (cond (((suit? suit) c) (suit-strat c d))
	  (else (base-strat c d)))))

(define (suit? suit)
  (lambda (c) (find-suit suit c)))

(define (find-suit suit c)
  (cond  ((empty? c) #f)
	 ((member? suit (last c)) #t)
	 (else (find-suit suit (bl c)))))

(define (valentine c d)
  ((suit-strategy 'h (stop-at 19) (stop-at 17)) c d))
