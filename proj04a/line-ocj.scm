; A1

(define-class (line-obj line-of-text)
  (method (empty?) (null? line-of-text))
  (method (next) (let ((next-token (car line-of-text)))
		   (set! line-of-text (cdr line-of-text))
		   next-token))
  (method (put-back token) (set! line-of-text (cons token line-of-text)))
  )

(define (make-line-obj text)
  (instantiate line-obj text))
