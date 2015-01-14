; Yuan Fang Fu
; cs61a-ae
; Bogdan Balas
; cs61a-bg
; TA: Darren Kuo; Section: 13; Group: 08;


; Part II, Person A

; A6a.

(define jail (instantiate place 'jail))

; GO-DIRECTLY-TO method inserts into definition of the PERSON class

(define-class (person name place)
  (parent (basic-object)) ; Part 4a
  (initialize
   (ask self 'put 'strength 250) ; Part B7 defines class strength
   (ask self 'put 'money 100)
   (ask place 'enter self))
   (method (type) 'person)
   (method (person?) #t)
  (instance-vars
   (possessions '())
   (saying "")
   (MONEY 100)) ; Part A7a - money property
  (initialize
   (ask place 'enter self))
  (method (go-directly-to new-place) ; Part A6a - GO-DIRECTLY-TO
    (announce-move name place new-place)
    (for-each (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
    (ask place 'exit self)
    (set! place new-place)
    (ask new-place 'enter self))
  (method (GET-MONEY amount) ; Part A7a- Get money method
    (SET! MONEY (+ MONEY amount)) )
  (method (PAY-MONEY amount) ; Part A7a - Pay money method
	    (COND ((>= MONEY amount)
		   (SET! MONEY (- MONEY amount))
		   #T)
		  (ELSE #F)))
  (method (buy food-type) ; Part A8
    (let ((food (ask place 'sell self food-type)))
      (if food
  (begin
   (set! possessions (cons food possessions))
   (ask food 'change-possessor self))
  (error "cannot buy food" food-type) )))
  (method (eat) ; Part B6 - implementation of the eat method
    (for-each (lambda (food)
		(ask self 'put 'strength (+ (ask self 'strength)
					    (ask food 'calories) ))
		(ask self 'lose food)
		(ask place 'gone food))
	      (filter edible? possessions)))
  (method (type) 'person)
  (method (put-down thing) ;  put something down to add it to place
	  (if (memq thing (ask place 'things))
	      (error "Thing already here" (list place thing))
	      (begin (ask self 'lose thing)
		     (ask place 'appear thing))))
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
      (cond ((not (thing? thing)) (error "Not a thing" thing))
      ((not (memq thing (ask place 'things)))
       (error "Thing taken not at this place"
      (list (ask place 'name) thing)))
      ((memq thing possessions) (error "You already have it!"))
      (else
      (let ((may-take (ask thing 'may-take self)))
	(if may-take
	    (begin
	      (announce-take name may-take) ; add it to possessions
	      (set! possessions (cons REPLY possessions)) ; go through all the people at the place and see if they have it
	      (for-each
	       (lambda (pers)
		 (if (and (not (eq? pers self)) ; ignore myself
			  (memq may-take (ask pers 'possessions)))
		     (begin
		       (ask pers 'lose may-take)
		       (have-fit pers))))
	       (ask place 'people))) ; actually change the possessor
	    (ask may-take 'change-possessor self)
	    'taken)
	(ammounce-too-weak name thing) )))) ; announce is too weak to take
  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
	     (error "Can't go" direction))
	    (else
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self))))) )

; A6b.

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (initialize
   (ask self 'put 'strength 300)) ; Part B7 defines class strength
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)
  (method (notice person)
    (if (eq? behavior 'run)
	(let ((exits (ask (usual 'place) 'exits)))
	  (IF (NOT (NULL? EXITS))
	      (ASK SELF 'GO (PICK-RANDOM EXITS))))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
		(ask self 'take (car food-things))
		(set! behavior 'run)
		(ask self 'notice person)) )))) )

; A7a.  Money method- see part A6a for person definition

; A7b. Create restaurant class - this is all replaced by the modified class in part 9

;(define-class (restaurant name food-class food-price)
;  (parent (place name))
;  (method (menu) (list (ask food-class 'name) food-price)) ; menu method
;  (method (sell buyer order) ; sell method
;    (if (eq? order (ask food-class 'name))
;(if (ask buyer 'pay-money food-price)
;    (let ((food (instantiate food-class)))
;      (ask self 'appear food)
;      food)
;    #f)
;#f)) )

; A8. Buy method for person class - see Part A6a

;;;;;;;;;;;;;;;;;;; Part B

; B6.

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; definition of the food class

(define-class (food name calories)
  (parent (thing name))
  (initialize (ask self 'put 'edible? #t)))

; the edible? procedure:

(define (edible? thing)
  (ask thing 'edible?))

; definiton of a child class for food

(define-class (bagel)
  (parent (food 'bagel 30))
  (class-vars (name 'bagel)))

; for the implementation of the eat method, refer to Part A6a for class person definition

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

; For Part B4b

(define (person? obj)
  (ask obj 'person?))

(define (place? obj)
  (ask obj 'place?))

(define (thing? obj)
  (ask obj 'thing?))

; added for 2E

(define (name obj) (ask obj 'name))

(define (inventory obj)
  (if (person? obj)
      (map name (ask obj 'possessions))
      (map name (ask obj 'things))))

(define (whereis person)
  (let ((place (ask person 'place)))
    (ask place 'name)))

(define (owner thing)
  (if (person? (ask thing 'possessor))
      (let ((person (ask thing 'possessor)))
	(ask person 'name))
      (se (name thing) '(is not owned by anyone))))

; B7. definition of the police class

(define-class (police name place)
  (parent (person name place))
  (initialize (ask self 'set-talk "Crime does not pay")
	      (ask self 'put 'strength 500)) ; Part B7 define strengths
  (method (police?) #t)
  (method (notice new-person)
    (if (eq? (ask new-person 'type) 'thief)
	(begin
	  (ask self 'talk)
	  (for-each (lambda (thing) (ask self 'take thing))
		    (ask new-person 'possessions) )
	  (ask new-person 'go-directly-to jail)) )))

; Default strengths assigned to persons, see Part A6a, B7, A6b, for the different types of people

; police at 500, persons at 250, thieves at 300


; B8.  We need a modified take method in the person clas- see part A6a

; and the MAY-TAKE? method for the thing class is in part

(define-class (thing name)
  (parent (basic-object)) ; Part B4a
  (instance-vars (possessor 'no-one))
  (method (possessor) possessor)
  (method (type) 'thing)
  (method (change-possessor new-possessor)
	  (set! possessor new-possessor))
  (method (thing?) #t) ; Part B4b
  (method (may-take? who)
	  (cond ((eq? possessor 'no-one) self)
		((> (ask who 'strength) (ask possessor 'strength))
		 self)
		(else #f))))


; Part 9

(define-class (restaurant name food-class food-price)
  (parent (place name))
  (method (menu) (list (ask food-class 'name) food-price))
  (method (sell buyer order)
    (if (eq? order (ask food-class 'name))
(if (OR (POLICE? BUYER) (ask buyer 'pay-money food-price))
    (let ((food (instantiate food-class food-name)))
      (ask self 'appear food)
      food)
    #f)
#f)) )


; Rest of the code needed to work

(define-class (place name)
  (parent (basic-object)) ; Part B4
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (method (type) 'place)
  (method (place?) #t) ; Part B4
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)

  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) )
  


