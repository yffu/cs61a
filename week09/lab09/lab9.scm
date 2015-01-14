; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 13 Group: 08

; Q1

; first response --> (b)
; none of the lists that are appended are changed in themselves, a new list is created by append.

; second response --> (b c d)
; append! calls last-apir on x and find the last pair. The cdr of the last pair is set to point at the pair representing y. X itself is changed to represent the entire list, and its cdr points to the sdr of the whole list.

; Q2

; (cdr x) returns the value held in the second box of the pair defined by x. You are then trying to change the value of a self-evaluating expression, namely 3, to a pointer value. Set-cdr! operates only on the location where the value of 3 is held, not on the value which it holds.

; Q3a

(define list1 (list (list 'a) 'b))

(define list2 (list (list 'x) 'y))

(set-cdr! (car list2)  (cdr list1))

(set-cdr! (car list1) (car list2))

(display list1)

(newline)

(display list2)

(newline)

; Q3b

; predicts that all instances of b are changed to y, as the place which held b has swapped it for the value of y.

; list1 --> ((a x y) y)

; list2 --> ((x y) y)

(set-car! (cdr list1) (cadr list2))

(display list1)

(newline)

(display list2)

(newline)

; Q4

; Ex. 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; creates a cycle, lols
; (last-pair z) would never find the null set, since any number of cdrs to z would still point to another pair.

; Ex. 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

; mystery takes a list and reverses the order of the list, using set!. Each iteration of the loop saves the cdr of x in temp, and changes the cdr of x to the acccumulated value of y. In effect adding the car of the cdr of x onto y, which is returned when the loop goes through an entire list. 
