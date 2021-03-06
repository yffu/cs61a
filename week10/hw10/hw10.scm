; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 13 Group: 08

; Q1

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

; Ex. 3.50

(define (stream-map-1 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams)) 
       (apply stream-map-1
	      (cons proc (map stream-cdr argstreams))))))
  
; test for stream-map-1

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

; Ex. 3.51

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (show x)
  (display-line x)
  x)

; STk> (define x (stream-map show (stream-enumerate-interval 0 10)))
;
; 0x

; the first element is printed due to the evaluation of the stream-car, which calls (show x) and causes 0 to be printed

; STk> (stream-ref x 5)
;
; 1
; 2
; 3
; 4
; 55

; as each element before 5 is subsequently forced in order to get to the final reference, each time (show x) is evaluated to give the memoized value for the let within the delay function. This is why each one is printed as the list goes down to 5. at five, stream-map tells (show x) to print 5, and returns its value to stream-ref, which, at the end of its loop, returns 5 again as the value of (stream-car x)  

; STk> (stream-ref x 7)
;
; 6
; 77

; memomized values are returned without calling the evaluation procedure again. No repeated values are printed.

; Ex. 3.52

(define sum 0)
(define (accum x)
  (set! sum (+ sum x))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))

; sum --> 1
; since accum is evaluated once by map-stream before the loop is delayed

(define y (stream-filter even? seq))

; sum --> 6
; stream-filter goes through 

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
			 seq))
;> sum --> 10
;> (stream-ref y 7) --> 136
;> (display-stream z)
; 0
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210done

; sum --> 210

; Ex. 3.53

(define (add-streams s1 s2)
  (stream-map-1 + s1 s2))

(define s (cons-stream 1 (add-streams s s)))

; is an alternate definition for the doubler

; STk> (stream-ref s 3)
; 8
; STk> (stream-ref s 4)
; 16

; Ex. 3.54

(define (mul-streams stream1 stream2)
  (stream-map-1 * stream1 stream2))

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

; Ex. 3.55

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

; Ex. 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))

(define (scale-stream stm factor)
  (cons-stream (* factor (stream-car stm))
	       (scale-stream (stream-cdr stm) factor)))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; Ex. 3.64

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)

(define (stream-limit stm tol)
  (let ((s1 (stream-car stm))
	(s2 (stream-cdr stm)))
    (if (> tol (abs (- s1 (stream-car s2))))
	s1
	(stream-limit s2 tol))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; Ex. 3.66

; (1, 100) preceded by 199 pairs, since the mapping of stream-cdr for T a fixed stream-car of S occurs every other function forced within interweave, as dictated by its alternating structure.

; (99, 100) (0, 1) before (1, 1), each successive value of s needs three pair evaluations to take place in order to be found in a new function within pairs. It takes 2^99 calls to pair to get to (99, 99). Each application of stream-cdr to the (map lambda (x) ....) expression in interweave needs 2^98 additional calls to set the value of the (cdr-stream x) to 100. (2^99) + (2^98)

; (100, 100) requires 100 initial pairs of the type (Tn, Sn) to be constructed, within each call to an existing pair function (represented by a specific value in S), the first forced evaluation causes interweave to extend the evaluation of the stream-map of pair-construction for a constant value of S. the second forced call to interweave causes the evaluation of the starting value of the successive value of S. This pattern alternates, so it takes two stream-cdrs to cause evaluation at the next level, so to speak. So it takes 2^100 calls to evaluate (100, 100).

; Ex. 3.68

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define (pairs-1 s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs-1 (stream-cdr s) (stream-cdr t))))

; the problem with this version is that interweave is not used to increment the value of the S value, so the series continues as (Sn, Tn), (Sn, Tn+1), etc...
; and more importantly, it doesn't even work, as interleave calls pair-1, and pair-1 calls interleave in a loop, due to the lack of a cons-stream.

; Q2

(define (get-digit numer denom)
  (if (> numer denom)
      (+ 1 (get-digit (- numer denom) denom))
      0))

(define (fract-stream fract-list)
  (cons-stream (quotient (* (car fract-list) 10) (cadr fract-list))
	       (fract-stream (list (remainder (* (car fract-list) 10)
					      (cadr fract-list))
				   (cadr fract-list)))))

(define (approximation str num)
  (if (= num 0)
      '()
      (cons (stream-car str) (approximation ( stream-cdr str) (- num 1))) ))
	       
; Q3

(define (im who message) ;Q3
  ;;;Send message to who.
  (if (list? who)
      (for-each (lambda (user) (im user messsage)) who)
  (if (not (send-request (make-request whoiam who 'send-msg message)
			 port-to-serve))
      (close-connection))))

; Q4

(define (broadcast message)
  (if (not (send-request (make-request whoiam 'server 'broadcast message) port-to-server))
      (close-connection)))



