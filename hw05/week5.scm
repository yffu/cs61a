;;2.37 - matrix operations

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define (matrix-*-vector m v)
  (map (lambda(row) (accumulate + 0 (map * row v))) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(row)
	   (cols-row row cols))
	   m)))

(define (cols-row row cols)
  (if (null? cols)
      nil
      (cons (dot-product row (car cols)) (cols-row row (cdr cols)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

