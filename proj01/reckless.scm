;P9

(define (reckless strategy)
  (lambda (c d)
    (strategy (bl c) d)))

(define (reckless-valentine c d)
  ((reckless valentine) c d))
