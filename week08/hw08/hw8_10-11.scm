;;Exercise 3.1.  An accumulator is a procedure that is called repeatedly with a single numeric argument and accumulates its arguments into a sum. Each time it is called, it returns the currently accumulated sum. Write a procedure make-accumulator that generates accumulators, each maintaining an independent sum. The input to make-accumulator should specify the initial value of the sum; for example

;;(define A (make-accumulator 5))
;;(A 10)
;;15
;;(A 10)
;;25

;;Exercise 3.2.  In software-testing applications, it is useful to be able to count the number of times a given procedure is called during the course of a computation. Write a procedure make-monitored that takes as input a procedure, f, that itself takes one input. The result returned by make-monitored is a third procedure, say mf, that keeps track of the number of times it has been called by maintaining an internal counter. If the input to mf is the special symbol how-many-calls?, then mf returns the value of the counter. If the input is the special symbol reset-count, then mf resets the counter to zero. For any other input, mf returns the result of calling f on that input and increments the counter. For instance, we could make a monitored version of the sqrt procedure:

;;(define s (make-monitored sqrt))

;;(s 100)
;;10

;;(s 'how-many-calls?)
;;1

;;===>>>Exercise 3.3.  Modify the make-account procedure so that it creates password-protected accounts. That is, make-account should take a symbol as an additional argument, as in
;;(define acc (make-account 100 'secret-password))

;;The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint:

;;((acc 'secret-password 'withdraw) 40)
;;60

;;((acc 'some-other-password 'deposit) 50)
;;"Incorrect password"

(define (make-acct balance passwd)
  (define (withdraw amount pass)
    (if (eq? pass passwd)
     (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")
	  "Incorrect password"))
  (define (deposit amount pass)
    (if (eq? pass passwd)
	(begin (set! balance (+ balance amount))
	       balance)
	  "Incorrect password"))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; a better version
(define (make-acct2 balance passwd)
  (define (withdraw amount)
     (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
	(begin (set! balance (+ balance amount))
	       balance))
  (define (incorrect_pass amount)
    "Incorrect password!")
  (define (dispatch pass m)
    (if (eq? pass passwd)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
    incorrect_pass))
  dispatch)

;;===>>>Exercise 3.4.  Modify the make-account procedure of exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure call-the-cops.
(define (call-the-cops)
  (display "Call the cops, intrusion detected..."))

(define (make-account balance passwd conseq-try)
  (define (withdraw amount pass)
    (if (eq? pass passwd)
    (begin (set! conseq-try 0)
     (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
    (begin (set! conseq-try (+ conseq-try 1))
      (if (>= conseq-try 7)
	  (call-the-cops)
	  99999))
    ))
  (define (deposit amount pass)
    (if (eq? pass passwd)
    (begin (set! conseq-try 0)
    (set! balance (+ balance amount))
    balance)
    (begin (set! conseq-try (+ conseq-try 1))
      (if (>= conseq-try 7)
	  (call-the-cops)
	  99999))
    ))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)


;;Exercise 3.7.  Consider the bank account objects created by make-account, with the password modification described in exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the make-joint operation to proceed. The third argument is a new password. Make-joint is to create an additional access to the original account using the new password. For example, if peter-acc is a bank account with password open-sesame, then


;;(define paul-acc
;;  (make-joint peter-acc 'open-sesame 'rosebud))

;;will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to exercise 3.3 to accommodate this new feature.

(define (make-joint base_acc base_passwd alias_passwd)
  (define (report_incorrect_pass balance) "Incorrect password for joint account!")
  (lambda(p m) (if (eq? p alias_passwd)
		   (base_acc base_passwd m)
		   report_incorrect_pass)
	 ))
		  
;;Exercise 3.8.  When we defined the evaluation model in section 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result. Define a simple procedure f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.

(define f
  (let ((count 1))
    (lambda(x) (if (= x 1)
	(begin (set! count 0) count)
	count))))

;;Exercise 3.10.  In the make-withdraw procedure, the local variable balance is created as a parameter of make-withdraw. We could also create the local state variable explicitly, using let, as follows:

(define (make-withdraw-orig balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (display balance) (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-withdraw-let initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (display initial-amount) (display balance) (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;;Recall from section 1.3.2 that let is simply syntactic sugar for a procedure call:

;;(let ((<var> <exp>)) <body>)

;;is interpreted as an alternate syntax for

;;((lambda (<var>) <body>) <exp>)

;;Use the environment model to analyze this alternate version of make-withdraw, drawing figures like the ones above to illustrate the interactions

;;(define W1 (make-withdraw 100))
;;(define W2 (make-withdraw 100))
;;(W1 50)
;;(W1 30)
;;(W2 50)

;; ==== make-withdraw-let version ========

;;(1) (define make-withdraw ...)
;;(2) (define W1 (make-withdraw 100))
;;(3) (define W2 (make-withdraw 100))

;;(4) (W1 50) started
;;
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-withdraw ...                                         |
;;global env =>| W2 o------------------------------------+                 |
;;             | W1 o                                    |                 |
;;             +----|------------------------------------|-----------------+
;;                  |            ^                       |            ^
;;                  |            |                       |            |
;;                  |            |                       |            |
;;                  |        +---------------------+     |       +------------------+
;;                  |        |   init-amount: 100  |     |       | init-amount: 100 |
;;                  |    E1=>|   balance: 100      |     |   E2=>| balance: 100     |
;;                  |        +---------------------+     |       +------------------+
;;                  |            ^           ^           |            ^
;;                  V            |           |           V            |
;;                param: amount -+    +-------------+  param: amount -+
;;                [body:code]         | amount: 50  |  [body:code]
;;                                    +-------------+
;;                                     balance (100) > amount (50) => set! balance (- balance amount)
;;
;;(5) (W1 50) completes and return 50
;;
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-withdraw ...                                         |
;;global env =>| W2 o------------------------------------+                 |
;;             | W1 o                                    |                 |
;;             +----|------------------------------------|-----------------+
;;                  |            ^                       |            ^
;;                  |            |                       |            |
;;                  |            |                       |            |
;;                  |        +---------------------+     |       +------------------+
;;                  |        |   init-amount: 100  |     |       | init-amount: 100 |
;;                  |    E1=>|   balance: 50       |     |   E2=>| balance: 100     |
;;                  |        +---------------------+     |       +------------------+
;;                  |            ^                       |            ^
;;                  V            |                       V            |
;;                param: amount -+                     param: amount -+
;;                [body:code]                          [body:code]
;;
;;(6) (W1 30) started
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-withdraw ...                                         |
;;global env =>| W2 o------------------------------------+                 |
;;             | W1 o                                    |                 |
;;             +----|------------------------------------|-----------------+
;;                  |            ^                       |            ^
;;                  |            |                       |            |
;;                  |            |                       |            |
;;                  |        +---------------------+     |       +------------------+
;;                  |        |   init-amount: 100  |     |       | init-amount: 100 |
;;                  |    E1=>|   balance: 50       |     |   E2=>| balance: 100     |
;;                  |        +---------------------+     |       +------------------+
;;                  |            ^           ^           |            ^
;;                  V            |           |           V            |
;;                param: amount -+    +-------------+  param: amount -+
;;                [body:code]         | amount: 30  |  [body:code]
;;                                    +-------------+
;;                                     balance (50) > amount (30) => set! balance (- balance amount)
;;
;;(7) (W1 30) completes and return 20
;;
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-withdraw ...                                         |
;;global env =>| W2 o------------------------------------+                 |
;;             | W1 o                                    |                 |
;;             +----|------------------------------------|-----------------+
;;                  |            ^                       |            ^
;;                  |            |                       |            |
;;                  |            |                       |            |
;;                  |        +---------------------+     |       +------------------+
;;                  |        |   init-amount: 100  |     |       | init-amount: 100 |
;;                  |    E1=>|   balance: 20       |     |   E2=>| balance: 100     |
;;                  |        +---------------------+     |       +------------------+
;;                  |            ^                       |            ^
;;                  V            |                       V            |
;;                param: amount -+                     param: amount -+
;;                [body:code]                          [body:code]
;;
;;(8) (W2 50) started
;;
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-withdraw ...                                         |
;;global env =>| W2 o------------------------------------+                 |
;;             | W1 o                                    |                 |
;;             +----|------------------------------------|-----------------+
;;                  |            ^                       |            ^
;;                  |            |                       |            |
;;                  |            |                       |            |
;;                  |        +---------------------+     |       +------------------+
;;                  |        |   init-amount: 100  |     |       | init-amount: 100 |
;;                  |    E1=>|   balance: 20       |     |   E2=>| balance: 100     |
;;                  |        +---------------------+     |       +------------------+
;;                  |            ^                       |            ^      ^
;;                  V            |                       V            |      |
;;                param: amount -+                     param: amount -+  +------------+
;;                [body:code]                          [body:code]       | amount: 50 |
;;                                                                       +------------+
;;                                                                  balance (100) > amount (50) => set! balance (- balance amount)
;;(9) (W2 50) completes and return 50
;;
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-withdraw ...                                         |
;;global env =>| W2 o------------------------------------+                 |
;;             | W1 o                                    |                 |
;;             +----|------------------------------------|-----------------+
;;                  |            ^                       |            ^
;;                  |            |                       |            |
;;                  |            |                       |            |
;;                  |        +---------------------+     |       +------------------+
;;                  |        |   init-amount: 100  |     |       | init-amount: 100 |
;;                  |    E1=>|   balance: 20       |     |   E2=>| balance: 50      |
;;                  |        +---------------------+     |       +------------------+
;;                  |            ^                       |            ^      
;;                  V            |                       V            |      
;;                param: amount -+                     param: amount -+  
;;                [body:code]                          [body:code]       
;;                                                                       
;;
;;



;;Show that the two versions of make-withdraw create objects with the same behavior. How do the environment structures differ for the two versions?

;; the environment model frame stack change are similar betweem make-withdraw-orig and make-withdraw-let, except that for let version: the formal parameter intial-amount keep the original value (100) bound to it when make-withdraw-let is called, the set! will affecting variable balance, with make-withdraw-orig, the set! directly operate on formal paramter balance.

;; the following are the runtime output that can support the above analysis:

;;======== begin output =======
;;STk> (define Z1 (make-withdraw-orig 100))
;;z1
;;STk> (define Z2 (make-withdraw-orig 100))
;;z2
;;STk> (Z1 50)
;;10050 -- note the output is 100 <- the balance before set!, 50 <- the result
;;STk> (Z1 40)
;;5010
;;STk> (Z2 50)
;;10050
;;STk> (Z2 30)
;;5020
;;STk> (define ZZ1 (make-withdraw-let 100))
;;zz1
;;STk> (define ZZ2 (make-withdraw-let 100))
;;zz2
;;STk> (ZZ1 50)
;;10010050 -- note the output are 100 <- formal param initial-amount, 100 <- the local var balance before set!, 50 <- the result
;;STk> (ZZ1 30)
;;1005020 -- note the output are 100 <- formal param initial-amount, 50 <- the local var balance before set!, 20 <- the result
;;STk>

;;======== end output ==========

;;Exercise 3.11.  In section 3.2.3 we saw how the environment model described the behavior of procedures with local state. Now we have seen how internal definitions work. A typical message-passing procedure contains both of these aspects. Consider the bank account procedure of section 3.1.1:

;;(define (make-account balance)
;;  (define (withdraw amount)
;;    (if (>= balance amount)
;;        (begin (set! balance (- balance amount))
;;               balance)
;;        "Insufficient funds"))
;;  (define (deposit amount)
;;    (set! balance (+ balance amount))
;;    balance)
;;  (define (dispatch m)
;;    (cond ((eq? m 'withdraw) withdraw)
;;          ((eq? m 'deposit) deposit)
;;          (else (error "Unknown request -- MAKE-ACCOUNT"
;;                       m))))
;;  dispatch)

;;Show the environment structure generated by the sequence of interactions

;;(1) ===> (define acc (make-account 50))

;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-account ...                                          |
;;global env =>|                                                           |
;;             | acc o                                                     |
;;             +-----|-----------------------------------------------------+
;;                   |            ^                       
;;                   |            |
;;                   |            |
;;                   |        +-----------------+
;;                   |        |   balance: 50   |
;;                   |    E1=>|   <deposit>     |
;;                   |        |   <withdraw>    |
;;                   |        |   <dispatch>    |
;;                   |        +-----------------+
;;                   |            ^
;;                   V            |
;;                param: amount --+
;;                [body:code]    
;;                [def deposit]  
;;                [def withdraw] 
;;                [def dispatch] 
;;                               
;;                               
;;                               
;;(2) ((acc 'deposit) 40) started
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-account ...                                          |
;;global env =>|                                                           |
;;             | acc o                                                     |
;;             +-----|-----------------------------------------------------+
;;                   |            ^                       
;;                   |            |
;;                   |            |
;;                   |        +-----------------+
;;                   |        |   balance: 50   |
;;                   |    E1=>|   <deposit>o-------------+
;;                   |        |   <withdraw>    |        |
;;                   |        |   <dispatch>o------+     |
;;                   |        +-----------------+  |     |
;;                   |            ^           ^    |     |   
;;                   V            |           |    V     |  
;;                param: amount --+   +-------------+    |
;;                [body:code]     E2=>| m: dispatch |    |
;;                [def deposit]       +-------------+    |
;;                [def withdraw]                 ^       |
;;                [def dispatch]                 |       V
;;                                          +--------------+
;;                                     E3=> | amount: 40   |
;;                                          +--------------+
;; (3) ((acc 'deposit) 40) completes and return 90
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-account ...                                          |
;;global env =>|                                                           |
;;             | acc o                                                     |
;;             +-----|-----------------------------------------------------+
;;                   |            ^                       
;;                   |            |
;;                   |            |
;;                   |        +-----------------+
;;                   |        |   balance: 90   |
;;                   |    E1=>|   <deposit>     |
;;                   |        |   <withdraw>    |
;;                   |        |   <dispatch>    |
;;                   |        +-----------------+
;;                   |            ^
;;                   V            |
;;                param: amount --+
;;                [body:code]     
;;                [def deposit]   
;;                [def withdraw]  
;;                [def dispatch]  

;;(4) ((acc 'withdraw) 60) started
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-account ...                                          |
;;global env =>|                                                           |
;;             | acc o                                                     |
;;             +-----|-----------------------------------------------------+
;;                   |            ^                       
;;                   |            |
;;                   |            |
;;                   |        +-----------------+
;;                   |        |   balance: 90   |
;;                   |    E1=>|   <deposit>     |
;;                   |        |   <withdraw>o------------+
;;                   |        |   <dispatch>o------+     |
;;                   |        +-----------------+  |     |
;;                   |            ^           ^    |     |   
;;                   V            |           |    V     |  
;;                param: amount --+   +-------------+    |
;;                [body:code]     E2=>| m: withdraw |    |
;;                [def deposit]       +-------------+    |
;;                [def withdraw]                 ^       |
;;                [def dispatch]                 |       V
;;                                          +--------------+
;;                                     E3=> | amount: 30   |
;;                                          +--------------+
;;(5) ((acc 'withdraw) 30) completes and return 60
;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-account ...                                          |
;;global env =>|                                                           |
;;             | acc o                                                     |
;;             +-----|-----------------------------------------------------+
;;                   |            ^                       
;;                   |            |
;;                   |            |
;;                   |        +-----------------+
;;                   |        |   balance: 60   |
;;                   |    E1=>|   <deposit>     |
;;                   |        |   <withdraw>    |
;;                   |        |   <dispatch>    |
;;                   |        +-----------------+
;;                   |            ^
;;                   V            |
;;                param: amount --+
;;                [body:code]     
;;                [def deposit]   
;;                [def withdraw]  
;;                [def dispatch]  


;;Where is the local state for acc kept? Suppose we define another account

;;ANSWER: the local state for acc is kept in E1 as shown above

;;(define acc2 (make-account 100))
;;
;; environment after define acc2...

;;             +-----------------------------------------------------------+
;;             |                                                           |
;;             | make-account ...                                          |
;;global env =>| acc2 o-------------------------------------+              |
;;             | acc o                                      |              |
;;             +-----|--------------------------------------|--------------+
;;                   |            ^                         |           ^
;;                   |            |                         |           |
;;                   |            |                         |           |
;;                   |        +-----------------+           |     +--------------+
;;                   |        |   balance: 60   |           |     | balance: 100 |
;;                   |    E1=>|   <deposit>     |           |     | <deposit>    |
;;                   |        |   <withdraw>    |           | E2=>| <withdraw>   |
;;                   |        |   <dispatch>    |           |     | <dispatch>   |
;;                   |        +-----------------+           |     +--------------+
;;                   |            ^                         |           ^
;;                   V            |                         V           |
;;                param: amount --+                     param: amount --+
;;                [body:code]                           [body:code]
;;                [def deposit]                         [def deposit]
;;                [def withdraw]                        [def withdraw]
;;                [def dispatch]                        [def dispatch]

;;How are the local states for the two accounts kept distinct? Which parts of the environment structure are shared between acc and acc2?

;; local states for the two accounts, e.g. formal parameter balance are allocated in E1 and E2 respectively and holding their initial value and updated value afterwards, also the internal definitions entries are kept respectivly in E1 and E2, invoking internal defined procs from either accounts will be allocated sub frame on top of their respective frame, i.e. E1 for acc and E2 for acc2.

;; the make-account definition and entries for acc and acc2 are in a common global area.
