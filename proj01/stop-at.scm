;

(define (stop-at n)
  (lambda (c d)
   (if (< (best-total c) n) #t #f)))
