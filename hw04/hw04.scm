; Ex. 2.20

(define (same-parity x . y)
  (recursive-same-parity x y))

(define (recursive-same-parity x y)
  (cond ((null? y) (cons x nil))
	((even? (- x (car y))) (cons x (recursive-same-parity (car y) (cdr y))))
	(else (recursive-same-parity x (cdr y)))))

; Ex. 2.22

; Although the order in general is correct, the list produced does not terminate in a pair with the right half as nil. Instead, the initial value of answer, nil fills the left half of the firs pair, as each successive pair takes the growing list in the left half, and the car of y on the right helf. Answer never flattens out, and instead keeps branching from the left half of each successive pair.


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

