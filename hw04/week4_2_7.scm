;;Exercise 2.7.  Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:

;;(define (make-interval a b) (cons a b))

;;Define selectors upper-bound and lower-bound to complete the implementation.

;;Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

;;Exercise 2.9.  The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.

;;Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
      (error "divider interval spans 0...")
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (width-of x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;Exercise 2.12.  Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.

(define (make-center-percent c p)
  (make-interval (- c (/ (* c p) 100)) (+ c (/ (* c p) 100))))

(define (percent x)
  (* (/ (- (upper-bound x) (center x)) (center x)) 100))

;;Exercise 2.17.  Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

;;(last-pair (list 23 72 149 34))
;;(34)

;;Exercise 2.18.  Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

;;(reverse (list 1 4 9 16 25))
;;(25 16 9 4 1)

(define (last-pair l)
  (cond ((null? l) nil)
	((empty? l) nill)
	((= (length l) 1) l)
	(else (last-pair (cdr l)))))

(define (reverse l)
  (rev-iter l '()))

(define (rev-iter x result)
  (cond ((null? x) result)
	((= (length x) 0) result)
	(else (rev-iter (cdr x) (cons (car x) result)))))

(define (same-parity x . y)
  (same-parity-iter x y nil))

(define (same-parity-iter x y result)
  (cond ((null? y) (cons x result))
	((parity-eq? x (car y)) (same-parity-iter x (cdr y) (append result (cons (car y) nil))))
	(else (same-parity-iter x (cdr y) result)))
)

(define (parity-eq? x y)
  (cond ((and (even? x) (even? y)) #t)
	((and (odd? x) (odd? y) #t))
	(else #f)))
	 
;;Exercise 2.21.  The procedure square-list takes a list of numbers as argument and returns a list of the squares of those numbers.

;;(square-list (list 1 2 3 4))
;;(1 4 9 16)

;;Here are two different definitions of square-list. Complete both of them by filling in the missing expressions:

;;(define (square-list items)
;;  (if (null? items)
;;      nil
;;      (cons <??> <??>)))
;;(define (square-list items)
;;  (map <??> <??>))

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

;;Exercise 2.19.  Consider the change-counting program of section 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure first-denomination and partly into the procedure count-change (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.

;;We want to rewrite the procedure cc so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have lists that defined each kind of currency:

;;(define us-coins (list 50 25 10 5 1))
;;(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;;We could then call cc as follows:

;;(cc 100 us-coins)
;;292

;;To do this will require changing the program cc somewhat. It will still have the same form, but it will access its second argument differently, as follows:

;;(define (cc amount coin-values)
;;  (cond ((= amount 0) 1)
;;        ((or (< amount 0) (no-more? coin-values)) 0)
;;        (else
;;         (+ (cc amount
;;                (except-first-denomination coin-values))
;;            (cc (- amount
;;                   (first-denomination coin-values))
;;                coin-values)))))

;;Define the procedures first-denomination, except-first-denomination, and no-more? in terms of primitive operations on list structures. Does the order of the list coin-values affect the answer produced by cc? Why or why not?
(define us-coins (list 50 25 10 5 1))
(define us-coins-reverse (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

(define (no-more? l)
  (empty? l))

(define (first-denomination l)
  (car l))

(define (except-first-denomination l)
  (cdr l))

;; running the cc with different us-coins shows that cc will give the same result for given amount dispite the order of coins values:

;;STk> (load "week4_2_7.scm")
;;okay
;;STk> (cc 23 us-coins)
;;9
;;STk> (cc 23 us-coins-reverse)
;;9
;;STk> (cc 51 us-coins)
;;50
;;STk> (cc 100 us-coins)
;;292
;;STk> (cc 100 us-coins-reverse)
;;292
;;STk> (cc 51 us-coins-reverse)
;;50
;;STk>

(define (substitute l owd nwd f)
  (cond ((null? l) nil)
	((word? (car l)) (cons (f (car l) owd nwd) (substitute (cdr l) owd nwd f)))
	(else (cons (substitute (car l) owd nwd f) (substitute (cdr l) owd nwd f))))
  )

(define (replace-wd target-wd owd nwd)
  (cond ((eq? target-wd owd) nwd)
      (else target-wd)))

(define (replace-wd-list target-wd ol nl)
  (cond
   ((null? ol) target-wd)
   ((eq? target-wd (car ol)) (car nl))
      (else (replace-wd-list target-wd (cdr ol) (cdr nl)))))

(define (substitute2 l ol nl)
  (substitute l ol nl replace-wd-list))
  
(define (substitute1 l owd nwd)
  (substitute l owd nwd replace-wd))

;;STk> 
;;(load "week4_2_7.scm")
;;okay
;;STk> (substitute1 '((lead guitar) (base guitar) (rhythm guitar) drums) 'guitar 'axe)
;;((lead axe) (base axe) (rhythm axe) drums)
;;STk> (substitute2 '((4 calling birds) (3 french hens) (2 turtle doves)) '(1 2 3 4) '(one two three four))
;;((four calling birds) (three french hens) (two turtle doves))
;;STk> 
