; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 13 Group: 08

; Ex. 3.3 and 3.4

(define (make-account balance password)
  (let ((errors 0))
    (set! password (cons password nil))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE_ACCOUNT"
		       m))))
  (define (correct-password? pswd method) ; Ex. 3.4 & 3.5
    (if (memq pswd password)
	(begin (set! errors 0)
	       (dispatch method))
	(begin (set! errors (+ 1 errors))
	       (if (> errors 3)
		   (call-the-cops)
		   (error "Incorrect password" pswd)))))
  
  correct-password?))

(define (call-the-cops)
  (display "police reponse is on the way")
  (newline)
  (error "Password limit reached"))

; Ex. 3.7

(define (make-joint account pswd1 pswd2) ; tried this way to keep the passwords user specific
  (define (correct-password? password method)
    (if (eq? password pswd2)
	(account pswd1 method)
	(error "Incorrect password" pswd)))
  correct-password?)

; Ex. 3.8

(define f
  (let ((k 1))
    (lambda (arg)
	    (begin (set! k (* k arg))
		   k))))

; (+ (f 0) (f 1))

; Ex. 3.10

(define (make-withdraw initial-amount)
  (let (balance initial-amount)
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))
