; twenty-one (joker)

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

; make-ordered-deck

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) 'Joker 'Joker))

; shuffle deck (no change)

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

; best-total

(define (best-total cards)
  (best-total-iter cards 0 0 0))

(define (best-total-iter cards ace-cont joker-cont sum)
  (cond ((empty? cards) (wild-value ace-cont joker-cont sum))
	((is-ace? (first cards)) (best-total-iter (bf cards) (+ ace-cont 1) joker-cont sum))
	((is-joker? (first cards)) (best-total-iter (bf cards) ace-cont (+ joker-cont 1) sum))
	(else (best-total-iter (bf cards) ace-cont joker-cont (card-value (first cards) sum)))))

(define (is-ace? card)
  (member? 'a card))

(define (is-joker? card)
  (equal? card 'joker))

(define (card-value card sum)
  (cond ((member? (first card) '(j q k 1)) (+ sum 10))
	(else (+ sum (bl card)))))

(define (wild-value ace-cont joker sum)
  (cond ((= joker 0)
	 (cond ((= ace-cont joker 0) sum)
	       ((<= sum 10) (wild-value (- ace-cont 1) joker (+ sum 11)))
	       (else (wild-value (- ace-cont 1) 0 (+ sum 1)))))
	((= joker 1)
	 (cond ((> sum 9) 21)
	       ((> ace-cont 0) (wild-value (- ace-cont 1) joker (+ sum 11)))
	       (else (wild-value 0 0 (+ sum 11)))))
	(else 21)))

;P2

(define (stop-at-17 customer-hand-so-far dealer-up-card)
  (cond ((< (best-total customer-hand-so-far) 17) #t)
	(else #f)))

;P3

(define (play-n strategy n)
  (play-n-iter strategy n 0))

(define (play-n-iter strategy n wins)
  (if (= n 0) wins
      (play-n-iter strategy (- n 1) (+ wins (twenty-one strategy)))))

;P4
  
(define (dealer-sensitive customer-hand-so-far dealer-up-card)
  (cond ((and (member? (first dealer-up-card) '(7 8 9 1 a j q k))
	    (< (best-total customer-hand-so-far) 17))
	 #t)
	((and (member? (first dealer-up-card) '(2 3 4 5 6))
	    (< (best-total customer-hand-so-far) 12))
	 #t)
	(else #f)))

;P5

(define (stop-at n)
  (lambda (c d)
   (if (< (best-total c) n) #t #f)))

;P6

(define (valentine c d)
  (cond ((heart? c) ((stop-at 19) c d))
	(else ((stop-at 17) c d))))

(define (heart? c)
  (cond ((empty? c) #f)
	((member? 'h (last c)) #t)
	(else (heart? (bl c)))))

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

(define (valentine-1 c d)
  ((suit-strategy 'h (stop-at 19) (stop-at 17)) c d))

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

;P9

(define (reckless strategy)
  (lambda (c d)
    (strategy (bl c) d)))

(define (reckless-valentine c d)
  ((reckless valentine) c d))
