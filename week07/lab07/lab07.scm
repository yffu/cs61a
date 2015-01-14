; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 1308

; demo2.scm

(define-class (counter)
  (instance-vars (count 0))
  (class-vars (total 0))
  (method (next)
    (set! total (+ total 1))
    (set! count (+ count 1))
    (list count total)))

(define-class (person name)
  (instance-vars (previous '()))
  (method (say stuff)
	  (let ((result stuff))
	    (set! previous result)
	    result))
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name)))
  (method (repeat) (ask self 'say previous)))

(define-class (pigger name)
  (parent (person name))
  (method (pigl wd)
    (if (member? (first wd) '(a e i o u))
	(word wd 'ay)
	(ask self 'pigl (word (bf wd) (first wd))) ))
  (method (say stuff)
    (if (word? stuff)
	(ask self 'pigl stuff)
	(map (lambda (w) (ask self 'pigl w)) stuff))) )

(define-class (squarer)
  (default-method (* message message))
  (method (7) 'buzz) )

(define-class (counter)
  (instance-vars (count 0))
  (class-vars (total 0) (counters '()))
  (initialize (set! counters (cons self counters)))
  (method (next)
    (set! total (+ total 1))
    (set! count (+ count 1))
    (list count total)))

(define-class (pigger name)
  (parent (person name))
  (method (pigl wd)
    (if (member? (first wd) '(a e i o u))
	(word wd 'ay)
	(ask self 'pigl (word (bf wd) (first wd))) ))
  (method (say stuff)
    (if (word? stuff)
	(if (equal? stuff 'my) (usual 'say stuff) (ask self 'pigl stuff))
	(map (lambda (w) (ask self 'say w)) stuff))) )

; double-talker.scm

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat)))) 

; No, since asking the self to repeat, calls the parent class, repeat calls the say method, which is taken from the child class. This results in an infinite loop that goes back and forth from the child to the parent definition.

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se stuff stuff)))

; 


(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (usual 'say (se stuff stuff))))
