; Q1

(define a (mapreduce (lambda (ikvp)
	     (list (make-kv-pair (kv-key ikvp) 1)))
	   +
	   0
	   "/gutenberg/shakespeare"))

(stream-accumulate + 0 (stream-map cdr a))

; Q2

; a)

(define gutenberg-wordcounts
  (mapreduce (lambda (line-key-pair)
	       (define (make-word-pair sent word-pair-list)
		 (if (empty? sent)
		     word-pair-list
		     (make-word-pair (bf sent)
				     (cons (cons (first sent) 1) word-pair-list))))
	       (make-word-pair (kv-value line-key-pair) '()))
	     +
	     0
	     "/gutenberg/shakespeare"))

; b)

(define (find-max-mapper kv-pair)
  (list (make-kv-pair (first (kv-key kv-pair))
		      kv-pair)))

(define (find-max-reducer current so-far)
  (if (> (kv-value current) (kv-value so-far))
      current
      so-far))



(define frequent
  (stream-accumulate find-max-reducer (make-kv-pair 'foo 0)
		     (stream-map cdr
		     (mapreduce find-max-mapper find-max-reducer
				(make-kv-pair 'foo 0) gutenberg-wordcounts)))
  
; the mapreduce part gets all the largest words starting with a single type of word. 
		     
; c)

; generate a stream of words used only once
  
(define (find-once-mapper kv-pair)
  (if (= (kv-value kv-pair) 1)
      (list (make-kv-pair (first (kv-key kv-pair)) (kv-pair)))
      '()))

(define (find-once-reducer current next)
  (cons (kv-value current) (kv-value next)))

(define occurs-once
  (mapreduce find-once-mapper cons (make-kv-pair 'foo '()) gutenberg-wordcounts))

; Q3

; a)

(define (find-matched-mapper kv-pair pattern)
  (if (match? (kv-value kv-pair) pattern)
  (let ((line (kv-value kv-pair)))
    (cond ((atom? line)
	   (list (make-kv-pair line kv-pair)))
	  ((atom? (first line))
	   (list (make-kv-pair (first line) kv-pair)))
	  (else '()))))
  '())

(define find-matched-reducer cons-stream)

(define (make-matched pattern directory-name)
  (mapreduce (lambda (x) (find-matched-mapper x pattern)) find-matched-reducer (make-kv-pair 'foo '()) directory-name))
 
				 
