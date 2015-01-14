 (define (all-smaller? tree num)
(cond ((null? tree) #t)
((> (entry tree) num) #f)
(else (and (all-smaller? (cadr tree) num) (all-smaller? (caddr tree) num)))))
