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
