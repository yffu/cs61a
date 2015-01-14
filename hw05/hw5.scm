; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 13 Group: 08

; Ex. 2.24

; --->XX---X/
;     |    |
;     |    |
;     V    V
;     1    XX--->X/
;          |     |
;          |     V
;          V     XX--->X/
;          2     |     |
;                V     V
;                3     4

; (1  (2  (3  4)))

; Ex. 2.26

; (append x y) --> (1 2 3 4 5 6)
; (cons x y) --> ((1 2 3) 4 5 6)
; (list x y) --> ((1 2 3) (4 5 6))

; Ex. 2.29

(define (make-mobile left right) ; part a
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-structure branch)
  (car branch))

(define (length-structure branch)
  (car (cdr mobile)))

(define (total-weight mobile)  ; part b works for even simplest mobile
  (let ((left (left-branch mobile))
	(right (right-branch mobile))
	(+ (if (not (pair? (branch-structure left)))
	       (branch-structure  left)
	       (total-weight left))
	   (if (not (pair? (left-branch right)))
	       (branch-structure  right)
	       (total-weight right))))))

(define (balanced? mobile) ; part c
  (let ((left (left-branch mobile))
	(right (right-branch mobile))
	(torque (lambda (branch) (* (total-weight branch) (length-structure branch))))
	(if (= (torque left) (torque right))
	    (cond ((pair? left)
		   (if (pair? right) (and (balanced? left) (balanced? right))
		       (balanced? left)))
		  ((pair? right)
		   (if (pair? left) (and (balanced? right) (balanced? left))
		       (balanced? right)))
		  (else #t))
	    #f))))

(define (make-mobile left right) ; part d
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; not much needs to be changed in the function itself, except the names of the constructors and the selectors. The selectors would need to be modified to get data out of a pair instead of a list. This can be done by simply changing the right-branch and length-structure to have the value of cd 

; Ex. 2.30

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items nil))

(define (square x)
  (* x x))

(define (square-tree items) ; direct implementation
  (cond ((null? items) nil)
	((not (pair? items)) (square items))
	(else (cons (square-tree (car items)) (square-tree (cdr items))))))

(define (square-tree-map items)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree-map sub-tree)
	     (square sub-tree)))
       items))

; Ex. 2.31

(define (tree-map func tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map func sub-tree)
	     (func sub-tree)))
       tree))

(define (square-tree-abstract tree) (tree-map square tree))

; Ex. 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (set) (cons (car s) set))  rest)))))

; the set of all subsets containing elements of a list, can be divided into two groups:
; one is the set of all subsets created from the cdr of the list.
; the other is the set of all subsets with the car of list added to each set in the first group.
; car of the list is mapped to each element of the first group, and
; appended to the list of subsets of cdr list

; Ex. 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

; Ex. 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m) (map (lambda (v) (* (car m ) v)) v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m) (map (lambda (cols) (dot-product m cols)) cols)) m)))

; Ex. 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

; STk> (fold-right / 1 (list 1 2 3))
; 1.5

; STk> (fold-left / 1 (list 1 2 3))
; 0.166666666666667

; STk> (fold-right list nil (list 1 2 3))
; (1 (2 (3 ())))

; STk> (fold-left list nil (list 1 2 3))
; (((() 1) 2) 3)

; op should have the property of being associative in order to produce the same value for any sequence

; and commutative? dunno

; Ex. 2.54

(define (equal? a b)
  (cond ((not (or (pair? a) (pair? b))) (eq? a b))
	((and (pair? a) (pair? b))
	 (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
	(else #f)))

; Modified Calc

; Scheme calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
	((word? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (accumulate + 0 (cdr args))))))
	((eq? fn '*) (accumulate * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (accumulate * 1 (cdr args))))))
	((eq? fn 'first) (cond ((null? args) (error "Calc: no args to first"))
			       ((= (length args) 1) (first (car args)))
			       (else (first args))))
	((eq? fn 'butfirst) (cond ((null? args) (error "Calc: no args to butfirst"))
			       ((= (length args) 1) (butfirst (car args)))
			       (else (butfirst args))))
	((eq? fn 'last) (cond ((null? args) (error "Calc: no args to last"))
			       ((= (length args) 1) (last (car args)))
			       (else (last args))))
	((eq? fn 'butlast) (cond ((null? args) (error "Calc: no args to butlast"))
			       ((= (length args) 1) (butlast (car args)))
			       (else (butlast args))))
	(else (error "Calc: bad operator:" fn))))


