; Yuan Fang Fu
; login: cs61a-ae
; TA: Darren Kuo
; Section: 13

; Ex. 2.7

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

; Ex. 2.8

; The interval resulting from the subtraction of interval a from interval b has its bounds the maximum and minimum values possible from the subtraction of an value in interval a from interval b. Therefore, upper-bound-(b-a) = upper-bound-b - lower-bound-a; lower-bound-(b-a) = lower-bound-a - upper-bound-b

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
		 (- (lower-bound x) (upper-bound y))))

; Ex. 2.10

(define (div-interval x y)
  (if (<= (* (upper-bound x) (lower-bound y)) 0)
      'error
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

; Ex. 2.12

(define (make-center-percent center percent-tolerance)
  (let ((tol (* center percent-tolerance)))
    (make-interval (+ center tol) (- center tol))))

(define (center interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

(define (percent interval)
  (/ (- (upper-bound interval) (lower-bound interval)) (* 2 (center interval))))

; Ex. 2.17

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (last-pair items)
  (if (= (length items) 1)
      items
      (last-pair (cdr items))))

; Ex. 2.20

(define (same-parity x . y)
  (recursive-same-parity x y))

(define (recursive-same-parity x y)
  (cond ((null? y) (cons x nil))
	((even? (- x (car y))) (cons x (recursive-same-parity (car y) (cdr y))))
	(else (recursive-same-parity x (cdr y)))))

; Ex. 2.22

; Since the first things that are taken from the list of items (as from the left) are placed at the start of the answers list, so are the orders of the two lists reversed.

; The fix would not work either, since the last two elements of the items list will occupy either half of a pair. Therefore the list is not ended with a nil, making the last two elements in, a pair within a list

; Although the order in general is correct, the list produced does not terminate in a pair with the right half as nil. Instead, the initial value of answer, nil fills the left half of the firs pair, as each successive pair takes the growing list in the left half, and the car of y on the right helf. Answer never flattens out, and instead keeps branching from the left half of each successive pair.

; > (square-list (list 1 2 3 4))
;   ((((() . 1) . 4) . 9) . 16)

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

; Ex. 2.23

(define (for-each proc list)
  (cond ((null? list) (newline) #t)
	(else (proc (car list)) (for-each proc (cdr list)))))

; P2

(define (substitute list old new replace-func)
  (cond ((null? list) nil)
	((not (pair? list))
	 (replace-func list old new))
	(else (cons
	       (substitute (car list) old new replace-func)
	       (substitute (cdr list) old new replace-func)))))

(define (substitute1 list old new)
  (substitute list old new replace-wd))

(define (replace-wd wd old-wd new-wd)
  (if (equal? wd old-wd)
      new-wd
      wd))

(define (substitute2 list old new)
  (substitute list old new replace-listed-wd))

(define (replace-listed-wd wd old-list new-list)
  (cond ((null? old-list) wd)
	((equal? wd (car old-list)) (car new-list))
	(else (replace-listed-wd wd (cdr old-list) (cdr new-list)))))


