; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 1308

; Q1

(define-class (random-generator name)
  (instance-vars (count 0))
  (method (number)
	  (set! count (+ count 1))
	  (random name))
  (method (count)
	  count))

; Q2

(define-class (coke-machine size price)
  (instance-vars (balance 0) (content 0))
  (method (fill number) (if (< content (- size number))
		     (set! content (+ number content))
		     (set! content size)))
  (method (deposit amount)
	  (set! balance (+ balance amount))
	  balance)
  (method (coke)
	  (cond ((= content 0) '(MACHINE EMPTY))
		((< balance price) '(NOT ENOUGH MONEY))
		(else (begin
			(set! balance (- balance price))
			balance)))))

; Q3

(define (ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (shuffle deck)
  (if (null? deck)
  '()
  (let ((card (nth (random (length deck)) deck)))
    (cons card (shuffle (remove card deck))) )))

(define-class (deck)
  (instance-vars (cards (shuffle (ordered-deck))))
  (method (deal) (if (null? cards)
		     '()
		     (begin (set! cards (cdr cards))
			    (car cards))))
  (method (empty?) (if (null? cards)
		       #t
		       #f)))

; Q4

(define-class (miss-manners object)
  (method (please message argument) (ask object message argument)))


; end
