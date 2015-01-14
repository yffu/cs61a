; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Sect: 13 Group: 08

; Ex. 4.3

(define (eval exp env)
  (cond ((self-evaluating? exp) exp) ; takes care of numbers and strings
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	(else ((get 'eval (car exp)) exp env))))

; think about op name, type of data, and then implement the get.

; Ex. 4.6

(define (let-params exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (var-list params)
  (cond ((null? params) '())
	(else (cons (caar params)
		    var-list (cdr params)))))

(define (exp-list params)
  (cond ((null? params) '())
	(else (cons (cdar params)
		    exp-list (cdr params)))))

(define (let->combination exp)
  (cons (make-lambda (var-list (let-params exp))
		     (let-body exp))
	(exp-list (let-params exp))))

(define (let? exp)
  (tagged-list? exp 'let))

; to be added to the definition of eval....

; (cond ((let? exp) (eval (let->combination exp) env)))

; Ex. 4.7

; let-params can still be used, as can be let-body

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (expand-params (let-params exp) (let-body exp)))

(define (expand-params params body)
  (let ((first (car params))
	(rest (cdr params)))
    (cond ((null? rest) (cons (make-lambda (car first) body)
			      (cadr first)))
	  (else (cons (make-lambda (car first)
				   (expand-params (cdr params) body))
		      (cadr first))))))

; it is sufficient to simply add a clause to eval - the let statement is then expanded and evaluated from the body.

; (cond ((let*? exp) (eval (let*->nested-lets exp) env)))

; Ex. 4.10

; perhaps we can have a system where the data tag is placed at the end, it is easy to conceive of changing the predicates and selectors to accomodate for the change.

; Ex. 4.11

; represent a frame as a list of bindings.

(define (make-frame variables values)
  (cond ((null? variables) '())
	(else (cons (make-binding (car variables) (car values))
		    (make-frame (cdr variables) (cdr values))))))

(define make-binding cons)

(define get-var car)

(define get-val cdr)

(define (add-binding-to-frame! var val frame)
  (set! frame (cons (make-binding var val) frame)))

; extend environment stays the same

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
	     (env-loop (enclosing-environment env)))
	    ((eq? (get-var (car frame)) var)
	     (get-val (car frame)))
	    (else (scan (cdr frame)))))
	     
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(scan (first-frame env))))
  (env-loop env))

(define (set-variable-value var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
	     (env-loop (enclosing-environment env)))
	    ((eq? (get-var (car frame)) var)
	     (set-cdr! (car frame) val))
	    (else (scan (cdr frame)))))
	     
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame) (add-binding-to-frame var val frame))
	    ((eq? (get-val (car frame)) var)
	     (set-cdr! (car frame) val))
	    (else (scan (cdr frame)))))
    (scan frame)))

; Ex. 4.13

; implement a way to get rid of bindings.

(define (make-unbound! var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
	    ((eq? (get-var (car frame)) var)
	     (set-car! frame (cdr frame)))
	    (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
	(error "Variable already unbound" var)
	(scan (first-frame env))))
  (env-loop env))
    

; remove first instances of the variable, or returns an error.

; Ex. 4.14

; map calls the primitive version of map, when map is applied to the rest of the list instead of being evaluated like a compound experssion, the rest of the list is first evaluated as if the expression to be mapped is applied to all the rest of the arguments at once. Errors will then result.

; Ex. 4.15

; halt could never be used for all p and a, since the application of p to itslef would require ever subsequent evaluations of the procedure in order to determine wether the procedure halts. This results in a infinite loop, and an indeterminate case. 
