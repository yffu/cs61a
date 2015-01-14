; Yuan Fang fu
; cs61a-ae
; TA: Darren Kuo
; Section: 13 Group: 08

; Ex. 3.16

; -->XX-->XX-->X/
;    |    |    |
;    V    V    V
;    a    b    c   ; Returns 3

; -->XX-->XX
;    |    ||
;    V    VV
;    a    X/
;         |
;         V  ; Returns 4
;         b

; -->XX
;    ||
;    VV
;    XX
;    ||
;    VV
;    X/  ; Returns 7
;    |
;    V
;    a


(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

; Ex. 3.17 Depthwise traversal

(define (count-pairs-1 list)
  (let ((aux ()))
    (define (helper lst)
      (cond ((not (pair? lst)) 0)
	    ((memq lst aux) 0)
	    (else (begin (set! aux (cons lst aux))
			     (+ 1 (helper-2 lst))))))
    (define (helper-2 lst)
      (+ (helper (car lst)) (helper (cdr lst))))
    (helper list)))
      
; Ex. 3.21

; the value printed when the queue is evaluated is the pair with the front pointer to the front of the list specified by the queue, and the rear pointer to the last pair of the list. So the printed result is a list with the car as the whole list, and the cdr of the pair pointing to the last pair of the list.

(define (front-ptr queue) (cdr queue))

(define print-queue front-ptr)

; Ex. 3.25

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	record
	false)))
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (lookup-arbitrary keys table)
  (let ((reference (lookup (car keys) table)) ; lookup modified to return a whole table, instead of just its contents
	(cond (reference (if (not (pair? (cdr reference)))
			     (cdr reference) ; if 1d table
			     (lookup-arbitrary keys reference))) ; if 2d table
	      (else (if (pair? (cdr keys))
			(lookup-arbitrary (cdr keys) table) ; for next entry in key list
			false)))))) ; if list ends
	  
	   
; Ex. 3.27

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n1))
		 (fib (-n 2))))))

(define (memo-fib)
  (memoize (lambda (n)
	     (cond ((= n 0) 0)
		   ((= n 1) 1)
		   (else (+ (memo-fib (-n 1))
			    (memo-fib (-n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x))) ; or returns the first value that's not false, or it returns false
	      (insert! x result table)
	      result))))))

; ________________<-----+                                                                         
; | global-env   |<-----|-+                                                             
; | memo-fib-----|-----00 |                                                             
; | memoize------|-------00                                                             
; | make-table---|                                                                       
; |              |                                                                       
; |______________|                                                                      
;            |                                                         
;            +-[f: anonymous function in memo-fib]                     
;                |                   |                                 
;                |                   +----[table: make-table]          
;               00                         |      |                    
;               |                          |      |                    
;               V                          [x: 3] |                    
;          {params: x                             [pcr: (look-up x table)]
;           body: let (pcr (lookup....}              |                 
;                                                    |                 
;                               [result: (f x)]------+     00--+       
;                                           |              |   |       
;                                           +--------------|---+       
;                                                          V           
;                                                 {params: result      
;                                                  body: (insert! x .... }
;                                                                      

; couldn't be arsed to do the whole thing

; Memo-fib computes the fibonacci number in a number of steps proportional to n, since the calculation of every subsequent fibonacci number requires a calculation involving all the previous numbers. Since the previous number is already stored away by memoized-fib, the next number is computed by accessing the two previous numbers in the stored list and adding them together, instead of recursively calculating all numbers lower in the list.

; If memo-fib were defined as (memoize fib), the results of preceeding calculations would still be stored in the table, although the function would never use those numbers for the next calculaion, instead using fib to get them laboriously- kinda defeats the purpose


;; Q1

(define (vector-append vect-1 vect-2) ; vector index starts at zero, NOT AT 1
  (define (loop newvec n1 n2)
    (cond ((= n1 0)
	   newvec)
	  ((<= n2 0)
	   (begin (vector-set! newvec (- n1 1) (vector-ref vect-1 (- n1 1)))
		  (loop newvec (- n1 1) n2)))
	  (else (begin (vector-set! newvec (+ n1 n2 -1) (vector-ref vect-2 (- n2 1)))
		       (loop newvec n1 (- n2 1))))))
  (loop (make-vector (+ (vector-length vect-1) (vector-length vect-2))) (vector-length vect-1) (vector-length vect-2)))

; Q2

(define (vector-filter pred vector)
  (define (loop vector-1 n count)
    (if (= n 0)
	(loop-1 vector-1 (make-vector count) count 0)
	(if (pred (vector-ref vector (- n 1)))
	    (begin (vector-set! vector-1 count (vector-ref vector (- n 1)))
		   (loop vector-1 (- n 1) (+ count 1)))
	    (loop vector-1 (- n 1) count))))

 (define (loop-1 vector-1 vector-2 n count)
   (if (= count n)
       vector-2
       (begin (vector-set! vector-2 count (vector-ref vector-1 (- n count 1)))
	      (loop-1 vector-1 vector-2 n (+ count 1)))))
 (loop (make-vector (vector-length vector)) (vector-length vector) 0))

(define (greaterthan9 x)
  (> x 9))

; use (vector-filter greaterthan9 (vector 12 34 5 7 9 10 34 23 100)) to test

; Q3 

; a

(define (bubble-sort! vector)
  (define (helper counter-1)
    (if (= counter-1 0)
	vector
	(let ((n1 (vector-ref vector (- counter-1 1)))
	      (n2 (vector-ref vector counter-1)))
	  (if (> n1 n2)
	      (begin (vector-set! vector counter-1 n1)
		     (vector-set! vector (- counter-1 1) n2))
	      (helper (- counter-1 1))))))
  (define (recursive-sort counter)
    (if (= counter 0)
	vector
	(begin (helper counter)
	       (recursive-sort (- counter 1)))))
    
	      
  (recursive-sort (- (vector-length vector) 1)))


; 3 2 1 --> 2 1 3 --> 1 2 3

; b

; Each iteration of the helper brings the largest value to the last spot in the vector, and each iteration of recursive-sort brings the subset of all the values excluding the largest, to be sorted once again by helper. This continues until the last element, leaving the last element of each subset of the vector, as the largest of the set.

; c

; dunno, maybe (n - 1)*(n - 1) steps, 
