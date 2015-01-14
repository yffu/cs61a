; Lab04 09/16/2010

;P3

(define (*rat a b)
  (make-rational (* (numerator a) (numerator b)) (* (denominator a) (denominator b))))

(define (make-rational num den)
(cons num den))

(define (numerator rat)
(car rat))

(define (denominator rat)
(cdr rat))

;P5

(define (+rat a b)
  (make-rational (+ (* (numerator a) (denominator b))
		    (* (numerator b) (denominator a)) )
		 (* (denominator a) (denominator b))))
;P6

;ex. 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (average (x-point start) (x-point end)) (average (y-point start) (y-point end)))))

; ex. 2.3
	    
(define (make-rectangle center width height)
  (cons center (cons width height)))

(define (rect-width rectangle)
  (car (cdr rectangle)))

(define (rect-height rectangle)
  (cdr (cdr rectangle)))

(define (ract-center rectangle)
  (car rectangle))

(define (rectangle-perimeter rectangle)
  (* 2 (rect-width rectangle) (rect-height rectangle)))

(define (rectangle-area rectangle)
  (* (rect-width rectangle) (rect-height rectangle)))

; alternate definition

(define (make-rectangle-alt start-pt end-pt)
  (cons start-pt end-pt))

(define (start-point rectangle)
  (car rectangle))

(define (end-point rectangle)
  (cdr rectangle))

(define (rect-width-alt rectangle)
  (abs (- (x-point (start-point rectangle)) (x-point (end-point rectangle)))))

(define (rect-height-alt rectangle)
  (abs (- (y-point (start-point rectangle)) (y-point (end-point rectangle)))))

(define (rectangle-perimeter-alt rectangle)
  (* 2 (rect-width-alt rectangle) (rect-height-alt rectangle)))

(define (rectangle-area-alt rectangle)
  (* (rect-width-alt rectangle) (rect-height-alt rectangle)))

; ex. 2.4
