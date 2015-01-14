;;Exercise 2.74.  Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The company's computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable's president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters' needs while preserving the existing autonomy of the divisions.

;;Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division's personnel records consist of a single file, which contains a set of records keyed on employees' names. The structure of the set varies from division to division. Furthermore, each employee's record is itself a set (structured differently from division to division) that contains information keyed under identifiers such as address and salary. In particular:

;;a.  Implement for headquarters a get-record procedure that retrieves a specified employee's record from a specified personnel file. The procedure should be applicable to any division's file. Explain how the individual divisions' files should be structured. In particular, what type information must be supplied?

;;b.  Implement for headquarters a get-salary procedure that returns the salary information from a given employee's record from any division's personnel file. How should the record be structured in order to make this operation work?

;;c.  Implement for headquarters a find-employee-record procedure. This should search all the divisions' files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee's name and a list of all the divisions' files.

;;d.  When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?

;;IEInc_Shanghai.scm:
;;--------------------------
(define (install-shanghai-module)
  ;; internal routines
  (define (full-record emp_name)
    (map (lambda(rec)
	   (if (eq? (car rec) emp_name)
	       rec
	       nil)) emp_info))
  (define (salary-part emp_rec)
    (if (null? emp_rec)
	nil
       (car (cdr emp_rec))))
  (define (address-part emp_rec)
    (if (null? emp_rec)
	nil
       (cdr (cdr emp_rec))))
  ;; externally accessible
  (define (tag x) (attach-tag 'shanghai x))
  (put 'get-salary '(shanghai) salary-part)
  (put 'get-address '(shanghai) address-part)
  (put 'get-record 'shanghai
       (lambda (emp_name) (tag (full-record emp_name))))
  ;; empployee records - structure: emp_rec=(emp_name(string), salary=(base_sal(int), flex_pay(int), bonus(int)), address=(country(string), province(string), city(string), street(string), number(int), zip(int)))
  (define emp_info
    (list
     '(joe1 (3700 500 180) ("P.R.CHINA" "NA" "SHANGHAI" "NANJING RD" 12880 290010)) 
     '(jane1 (3600 500 280) ("P.R.CHINA" "NA" "SHANGHAI" "YANGTSZ RIVER AVE" 1070 290010)))) 
  'done)

;;--------------------------

;;IEInc_NewJersey.scm:
;;--------------------------
(define (install-NJ-module)
  ;; internal routines
  (define (full-record emp_name)
    (map (lambda(rec)
	   (if (eq? (car rec) emp_name)
	       rec
	       nil)) emp_records))
  (define (salary-part emp_rec)
    (if (null? emp_rec)
	nil
       (cdr (cdr emp_rec))))
  (define (address-part emp_rec)
    (if (null? emp_rec)
	nil
       (car (cdr emp_rec))))
  ;; externally accessible
  (define (tag x) (attach-tag 'newjersey x))
  (put 'get-salary '(newjersey) salary-part)
  (put 'get-record 'newjersey
       (lambda (emp_name) (tag (full-record emp_name))))
  ;; empployee records - structure: emp_rec=(emp_name(symbol), address=(country(string), state(string), city(string), street(string), number(int), zip(int)) salary=(base_sal(int), bonus(int)))
  (define emp_records
    (list
     '(joe2  ('USA "New Jersey" "Jersey City" "Woodhaven Ave" 17802 08512) (8000 1000)) 
     '(jane2  ('USA "New Jersey" "Jersey City" "Alameda Street" 7001 08517) (6700 1200)) 
  'done)
;;--------------------------

;;------- global data at headquarter ------
(define divisions
  '((shanghai "shanghai_emp_db.scm") (newjersey "us_emp_db.scm"))) ;; add new division, e.g. zurich & file here

;;---- begin headquarter's procedures -----
;;---- constructor ------------
(define (get-record emp_name location) ;; location can be: e.g. shanghai, newjersey, etc
   ((get 'get-record location) emp_name))
;;---- kinda selectors ---------
(define (get-salary emp_rec) (apply-generic 'salary-part emp_rec))
(define (get-address emp_rec) (apply-generic 'address-part emp_rec))
;;---- higher level routines ------
(define (find-employee-record emp_name . division_dbs)
  (if (null? division_dbs)
      nil
      (map (lambda(x) (get-record emp_name (car x))) division_dbs))) ;; note this will find all employees by the name across all the divisions
;; also note the result could contains employee records of different type (tagged)

;; modules loader
(define (loaddbs dbs)
  (if (null? dbs)
      nil
      ((load (last (car dbs)))
       (loaddbs (cdr dbs)))))

;; main code
(loaddbs division)
(find-employee-record 'joe1 division)


;; renamed to co-exists with original apply-generic
(define (apply-generic-IE-Inc op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))
  
;;---- end headquarter's procedures --------  

;; ==========================================
;; complex arithmetic - data directed edition
;; message passing edition commented out
;; ==========================================
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (square x)
  (* x x))

;; extract constructors from table
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Ben's package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
;; regular data directed style
  (define (make-from-real-imag x y) (cons x y))

;; message passing style
;;  (define (make-from-real-imag x y)
;;    (define (dispatch op)
;;      (cond ((eq? op 'real-part) x)
;;	    ((eq? op 'imag-part) y)
;;	    ((eq? op 'magnitude)
;;	    (sqrt (+ (square x) (square y))))
;;	    ((eq? op 'angle) (atan y x))
;;	    (else
;;	     (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;;    dispatch)

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
;; regular data directed style
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

;; message passing style
;;(define (make-from-mag-ang r a)
;;  (define (dispatch op)
;;    (cond ((eq? op 'real-part) (* r (cos a)))
;;          ((eq? op 'imag-part) (* r (sin a)))
;;          ((eq? op 'magnitude) r)
;;          ((eq? op 'angle) a)
;;          (else
;;           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;;  dispatch)

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
;; Alyssa's package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
;; regular data directed style
;;  (define (make-from-mag-ang r a) (cons r a))
;; message passing style
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
(define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
;; un-comments to revert to regular data directed style
;;  (define (make-from-real-imag x y) 
;;    (cons (sqrt (+ (square x) (square y)))
;;          (atan y x)))
  (define (make-from-real-imag x y)
    (define (dispatch op)
      (cond ((eq? op 'real-part) x)
	    ((eq? op 'imag-part) y)
	    ((eq? op 'magnitude)
	    (sqrt (+ (square x) (square y))))
	    ((eq? op 'angle) (atan y x))
	    (else
	     (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
    dispatch)
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; message passing style apply-generic
;; (define (apply-generic op arg) (arg op))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;Exercise 2.75.  Implement the constructor make-from-mag-ang in message-passing style. This procedure should be analogous to the make-from-real-imag procedure given above.

;;(define (make-from-real-imag x y)
;;  (define (dispatch op)
;;    (cond ((eq? op 'real-part) x)
;;          ((eq? op 'imag-part) y)
;;          ((eq? op 'magnitude)
;;           (sqrt (+ (square x) (square y))))
;;          ((eq? op 'angle) (atan y x))
;;          (else
;;           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;;  dispatch)

;;(define (make-from-mag-ang r a)
;;  (define (dispatch op)
;;    (cond ((eq? op 'real-part) (* r (cos a)))
;;          ((eq? op 'imag-part) (* r (sin a)))
;;          ((eq? op 'magnitude) r)
;;          ((eq? op 'angle) a)
;;          (else
;;           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;;  dispatch)

;; regular symbolic differenciation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
;; helpers
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;(define (make-sum a1 a2) (list '+ a1 a2)) ;; replaced by a better version

;;(define (make-product m1 m2) (list '* m1 m2)) ;; replaced by a better version

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; data-directed symbolic differenciation
;;(define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         (else ((get 'deriv (operator exp)) (operands exp)
;;                                            var))))
;;(define (operator exp) (car exp))
;;(define (operands exp) (cdr exp))
