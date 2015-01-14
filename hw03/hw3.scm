; Yuan Fang Fu
; cs61a-ae
; Section 13, TA Darren Kuo

;Problem 1 Ex. 1.16

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter (square b) (/ n 2) a))
	(else (fast-expt-iter b (- n 1) (* a b)))))

(define (square x)
  (* x x))

;Problem 1 Ex. 1.35

;phi = (1 + sqrt(5))/2
;(phi)^2 = (1/4)(6 + 2*sqrt(5))
;(phi)^2 = 1 + (1 + sqrt(5))/2

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
(define (try guess)
  (let ((next (f guess)))
    (if (close-enough? guess next)
	next
	(try next))))
(try first-guess))

(define golden-ratio
  (fixed-point (lambda(x) (+ 1 (/ 1 x))) 1.0))

; Problem 1 Ex. 1.37
; part a

(define (cont-frac n d k)
  (cont-frac-recur n d k 1))

(define (cont-frac-recur n d k i)
  (if (> i k)
      0
      (/ (n i) (+ (d i) (cont-frac-recur n d k (+ i 1))))))

; part b

(define (cont-frac-2 n d k)
  (cont-frac-iter n d k 0))

(define (cont-frac-iter n d k x)
  (cond ((= k 0) x)
	(else (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) x))))))   

; Problem 1 Ex. 1.38

(define (d i)
  (let ((x (remainder i 3)))
  (cond ((= x 2) (* 2 (/ (+ i 1) 3)))
	(else 1))))

(define (e-2 k)
  (cont-frac-2
   (lambda (i) 1.0)
   d
   k))

; P2
(define (next-perf n)
   (if (is-perf? n)
       n
       (next-perf (+ n 1))))

(define (is-perf? n)
  (= n (sum-of-factors n 0 1)))
  
(define (sum-of-factors n sum cont)
  (cond ((= cont n) sum)
	((is-fact? n cont) (sum-of-factors n (+ cont sum) (+ cont 1)))
	(else (sum-of-factors n sum (+ cont 1)))))

(define (is-fact? n d)
  (= 0 (remainder n d)))

; procedure returns 496 as next perfect number

; P3

;The change in the order of the base case conditions would result in the exclusion of combinations where all types of coins are used, from the count. Cases where the coin-type count is zero are automatically discredited, without verifying if the last coin had completed the amount exactly. The return value would be less in the changed case.

; P4

; Product * (b)^product = (b)^n
