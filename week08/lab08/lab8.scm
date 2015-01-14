; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 13 Group: 08

; Q1

(define (make-account balance)
  (define (withdraw amount)
    (set! balance (- balance amount)) balance)
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (dispatch msg)
    (cond ((eq? msg 'withdraw) withdraw)
	  ((eq? msg 'deposit) deposit)
	  (else (error "Method not found" msg))))
  dispatch)

(define (make-account-1 init-amount)
  (let ((balance init-amount))
    (define (withdraw amount)
      (set! balance (- balance amount)) balance)
    (define (deposit amount)
      (set! balance (+ balance amount)) balance)
    (define (dispatch msg)
      (cond
       ((eq? msg 'withdraw) withdraw)
       ((eq? msg 'deposit) deposit)))
    dispatch))

; Q2

(define (make-account-2 init-amount)
  (let ((balance init-amount)
	(init-balance init-amount))
    (define (withdraw amount)
      (set! balance (- balance amount)) balance)
    (define (deposit amount)
      (set! balance (+ balance amount)) balance)
    (define (dispatch msg)
      (cond
       ((eq? msg 'withdraw) withdraw)
       ((eq? msg 'deposit) deposit)
       ((eq? msq 'init-balance) init-balance))
    dispatch)))

; Q3

(define (make-account-3 init-amount)
  (let ((balance init-amount)
	(init-balance init-amount)
	(transactions ()))
    (define (withdraw amount)
      (set! balance (- balance amount))
      (set! transactions (cons (se msg amount) transactions))
      balance)
    (define (deposit amount)
      (set! balance (+ balance amount))
      (set! transactions (cons (se msg amount) transactions))
      balance)
    (define (dispatch msg)
      (cond
       ((eq? msg 'withdraw) withdraw)
       ((eq? msg 'deposit) deposit)
       ((eq? msg 'init-balance) (begin
				  (set! transactions
					(cons msg transactions))
				  init-balance))
       ((eq? msg 'transactions) transactions)))
    dispatch))

; Q4

(define (plus1 var)
  (set! var (+ var 1))
  var)

; (set! 5 (+ 5 1)) 5) should return an error, as self-evaluating variables are can't be set to another value. In scheme, (plus1 5) returns 6
