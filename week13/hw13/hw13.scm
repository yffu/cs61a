; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 13 Group: 08

; Ex. 4.25

; If (factorial 5) were evaluated in applicative-order scheme, an infinite loop would result, since unless tries to evaluate all of its clauses before pulling up the body of unless. This results in a call to factorial, and a call back to unless. The procedure call will work in a normal order language.

; Ex. 4.26

; A special form unless is possible to implement, where the evaluator could recognize a list starting with "unless", and perhaps implement it as an if statement in underlying scheme. But on the other hand, if unless were to be used by higher order functions such as map or accumulate, then the clause for compound procedures would cause unless to be evaluated by itself, resulting in an error.

; for example

(define (foo func pred usual alt)
  (func pred usual alt))

(foo unless #t 1 2)

; returns error

; Ex. 4.28



; Mapreduce Exercises

; Q1

; a)

(define (inverted-index-mapper kv-pair)
  (let ((document-name (kv-key kv-pair)))
    (map (lambda (wd-in-line)
	  (make-kv-pair wd-in-line document-name))
	 (kv-value kv-pair))))
	 
(; returns a list of kv-pairs whose keys are the word, and the value is another kv-pair with word name and the title of the document as its kv-value.					    
(define (inverted-index-reducer current next)
  (if (member? current next)
      next
      (cons current next)))
      

; mapreduce should return a stream of the different values provided by each reducer. In this case a list of documents associated with each word. Needs to return an index stream with all the keys.

(define (make-inverted-index directory)
  (mapreduce inverted-index-mapper inverted-index-reducer
	     '() directory))

(define word-title-stream (make-inverted-index "/gutenberg/shakespeare"))xs

; remember that mapper is the function applied by stream-map, and reducer is the one used by stream-accumulate

; b) get only the important words

;(define (important-word-mapper kv-pair min-length)
;  (define (helper rest filtered)
;    (cond ((empty? rest) filtered)
;	  ((>= (length (first rest)) min-length)
;	   (helper (bf rest)
;		   (cons (make-kv-pair (first rest)
;				       (kv-key kv-pair))
;			 filtered)))
;	  (else (helper (bf rest)
;			filtered))))
 ; (helper (kv-value kv-pair) '()))
;  (let ((title (kv-key kv-pair))
;	(filtered-line
;	 (filter (lambda (wd-in-line)
;		   (>= (length wd-in-line) min-length))
;		 (kv-value kv-pair))))
 ;   (if (null? filtered-line)
;	'()
;	(map (lambda (x) (make-kv-pair x title))
;	     filtered-line))))

(define (make-important-word-index filename min-length)
  (mapreduce (lambda (kv-pair) (map (lambda (wd) (make-kv-pair wd (kv-key kv-pair))) (filter (lambda (wd) (>= (count wd) min-length)) (kv-value kv-pair))))
	     inverted-index-reducer '() filename))

(define important (make-important-word-index "/gutenberg/shakespeare" 6))

; Q2

; produce a table with subject lines as keys and counts of occurences as values/ Identify the intermediate key-value pairs and write the mapper and reducer functions.

; use "/sample-emails"

; a)

(define (kv-subject-occurance-stream filename)
  (mapreduce (lambda (kv-pair) (list (make-kv-pair
				(cadddr kv-pair) 1)))
	     +
	     0
	     filename))

(define subject-frequency
  (kv-subject-occurance-stream "/sample-emails"))

; intermediate key-value pair is subject as key, value as the number 1

; b)

(define kv-subject-frequency-sort
  (mapreduce (lambda (kv-pair)
	       (list (make-kv-pair
		      (- (kv-value kv-pair))
		      (kv-key kv-pair))))
	     cons-stream
	     (make-kv-pair 'foo '())
	     subject-frequency))

(define (stream->list proc stream length)
  (define (helper remains so-far count)
    (if (= count 0)
	so-far
	(helper (stream-cdr remains)
		(cons (proc (stream-car remains))
		      so-far)
		(- count 1))))
  (helper stream '() length))

(define frequent-subjects
  (stream->list (lambda (x) (cadr x))
		kv-subject-frequency-sort
		10))

; c)

;assuming a list of top subject titles

(define (kv-address-occurance document frequent-list)
  (mapreduce (lambda (kv-pair)
	       (if (member? (cadddr kv-pair) frequent-list)
		   (list (make-kv-pair (cadr kv-pair) 1))
		   '()))
	     +
	     0
	     document))

(define sender-subject-frequency
  (kv-address-occurance "/sample-emails" frequent-subjects))

; the intermediate values are the sender addresses, and to sort sender address by subject frequency, the procedure from 2b can be used.

; Q3

; why does all have to be done in one step?
; The implicit sort function of the mapreduce would complicate things a bit, or would it?

; Although the mapreduce model takes up overhead in distibuting the processing of data pairs into different machines, especially for smaller tasks and trivial operations, it would be difficult to separate the separate steps of map and reduce. Although the implemantation of parallelism is not clear, it could be possible that the implicit sort performs in parallel with map, and would do so more efficiently where both map and reduce are integrated.

; Q4

; it doesn;t seem likely, since the contruction of the sieve would influence the type of values that each mapper would receive, and would depend on the accumulation of all sieve up to that point.
