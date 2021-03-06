CS 61A -- Week 9 Solutions


HOMEWORK:

3.16 incorrect count-pairs

This procedure would work fine for any list structure that can be expressed
as (quote <anything>).  It fails when there are two pointers to the same pair.

(define a '(1 2 3))                    (count-pairs a) --> 3

(define b (list 1 2 3))
(set-car! (cdr b) (cddr b))            (count-pairs b) --> 4

(define x (list 1))
(define y (cons x x))
(define c (cons y y))                  (count-pairs c) --> 7

(define d (make-cycle (list 1 2 3)))   (count-pairs d) --> infinite loop

Note from example c that it's not necessary to use mutators to create
a list structure for which this count-pairs fails, but it is necessary
to have a name for a substructure so that you can make two pointers to it.
The name needn't be global, though; I could have said this:

(define c
  (let ((x (list 1)))
    (let ((y (cons x x)))
      (cons y y) )))


3.17 correct count-pairs  

(define (count-pairs lst)
  (let ((pairlist '())
(count 0))
    (define (mark-pair pair)
      (set! pairlist (cons pair pairlist))
      (set! count (+ count 1)))
    (define (subcount pair)
      (cond ((not (pair? pair)) 'done)
    ((memq pair pairlist) 'done)
    (else (mark-pair pair)
  (subcount (car pair))
  (subcount (cdr pair)))))
    (subcount lst)
    count))

The list structure in pairlist can get very complicated, especially if
the original structure is complicated, but it doesn't matter.  The cdrs
of pairlist form a straightforward, non-circular list; the cars may point
to anything, but we don't follow down the deep structure of the cars.  We
use memq, which sees if PAIR (a pair) is eq? (NOT equal?) to the car of some
sublist of pairlist.  Eq? doesn't care about the contents of a pair; it just
looks to see if the two arguments are the very same pair--the same location
in the computer's memory.

[Non-experts can stop here and go on to the next problem.  The following
optional material is just for experts, for a deeper understanding.]

It's not necessary to use local state and mutation.  That just makes the
problem easier.  The reason is that a general list structure isn't a sequence;
it's essentially a binary tree of pairs (with non-pairs as the leaves).  So
you have to have some way to have the pairs you encounter in the left branch
still remembered as you traverse the right branch.  The easiest way to do
this is to remember all the pairs in a variable that's declared outside the
SUBCOUNT procedure, so it's not local to a particular subtree.

But another way to do it is to have a more complicated helper procedure
that takes PAIRLIST as an argument, but also sequentializes the traversal by
keeping a list of yet-unvisited nodes, sort of like the breadth-first tree
traversal procedure (although this goes depth-first because TODO is a stack,
not a queue):

(define (count-pairs lst)
  (define (helper pair pairlist count todo)
    (if (or (not (pair? pair)) (memq pair pairlist))        ; New pair?
        (if (null? todo)     ;  No. More pairs?
            count                                           ;   No. Finished.
            (helper (car todo) pairlist count (cdr todo)))  ;   Yes, pop one.
        (helper (car pair) (cons pair pairlist) (+ count 1) ;  Yes, count it,
                (cons (cdr pair) todo))))     ;  do car, push cdr
  (helper lst '() 0 '()))

As you're reading this code, keep in mind that all the calls to HELPER
are tail calls, so this is an iterative process, unlike the solution
using mutation, in which the call (SUBCOUNT (CAR PAIR)) isn't a tail call
and so that solution generates a recursive process.

And after you understand that version, try this one:

(define (count-pairs lst)
  (define (helper pair pairlist count todo)
    (if (or (not (pair? pair)) (memq pair pairlist))        ; New pair?
(todo pairlist count)     ; No. Continue.
        (helper (car pair) (cons pair pairlist) (+ count 1) ;  Yes, count it,
(lambda (pairlist count)     ;  do car, push cdr
  (helper (cdr pair) pairlist count todo)))))
  (helper lst '() 0 (lambda (pairlist count) count)))

Here, instead of being a list of waiting pairs, TODO is a procedure that
knows what tasks remain.  The name for such a procedure is a "continuation"
because it says how to continue after doing some piece of the problem.
This is an example of "continuation-passing style" (CPS).  Since TODO is
tail-called, you can think of it as the target of a goto, if you've used
languages with that feature.


3.21 print-queue 

The extra pair used as the head of the queue has as its car an ordinary
list of all the items in the queue, and as its cdr a singleton list of
the last element of the queue.  Each of Ben's examples print as a list of
two members; the first member is a list containing all the items in the
queue, and the second member is just the last item in the queue.  If you
look at what Ben printed, take its car and you'll get the queue items;
take its cdr and you'll get a list of one member, namely the last queue
item.  The only exception is Ben's last example.  In that case, the car of
what Ben prints correctly indicates that the queue is empty, but the cdr
still contains the former last item.  This is explained by footnote 22
on page 265, which says that we don't bother updating the rear-ptr when we
delete the last (or any) member of the queue because a null front-ptr is
good enough to tell us the queue is empty.

It's quite easy to print the sequence of items in the queue:

(define print-queue front-ptr)


3.25 multi-key table

Several students generalized the message-passing table implementation
from page 271, which is fine, but it's also fine (and a little easier)
to generalize the simpler version of page 270:

(define (lookup keylist table)
  (cond ((not table) #f)
((null? keylist) (cdr table))
(else (lookup (cdr keylist)
      (assoc (car keylist) (cdr table))))))

(define (insert! keylist value table)
  (if (null? keylist)
      (set-cdr! table value)
      (let ((record (assoc (car keylist) (cdr table))))
(if (not record)
    (begin
     (set-cdr! table
       (cons (list (car keylist)) (cdr table)))
     (insert! (cdr keylist) value (cadr table)))
    (insert! (cdr keylist) value record)))))

That solution assumes all the entries are compatible.  If you say
(insert! '(a) 'a-value my-table)
(insert! '(a b) 'ab-value my-table)
the second call will fail because it will try to
(assoc 'b (cdr 'a-value))
and the CDR will cause an error.  If you'd like to be able to have
values for both (a) and (a b), the solution is more complicated;
each table entry must contain both a value and a subtable.  In the
version above, each association list entry is a pair whose CAR is
a key and whose CDR is *either* a value or a subtable.  In the version
below, each association list entry is a pair whose CAR is a key and
whose CDR is *another pair* whose CAR is a value (initially #f) and whose
CDR is a subtable (initially empty).  Changes are in CAPITALS below:

(define (lookup keylist table)
  (cond    ; *** the clause ((not table) #f) is no longer needed
   ((null? keylist) (CAR table)) ; ***
   (else (LET ((RECORD (assoc (car keylist) (cdr table))))
   (IF (NOT RECORD)
       #F
       (lookup (cdr keylist) (CDR RECORD))))))) ; ***

(define (insert! keylist value table)
  (if (null? keylist)
      (SET-CAR! table value) ; ***
      (let ((record (assoc (car keylist) (cdr table))))
(if (not record)
    (begin
     (set-cdr! table
       (cons (LIST (CAR keylist) #F) (cdr table))) ; ***
     (insert! (cdr keylist) value (CDADR table)))
    (insert! (cdr keylist) value (CDR RECORD)))))) ; ***


Note:  In a sense, this problem can be solved without doing any work at all.
In a problem like

(lookup '(red blue green) color-table)

you can think of (red blue green) as a list of three keys, each of which is
a word, or as a single key containing three words!  So the original
one-dimensional implementation will accept this as a key.  However, for a
large enough table, this would be inefficient because you have to look
through a very long list of length Theta(n^3) instead of three lists each
Theta(n) long.



3.27  Memoization

Here's what happened when I tried it, with annotations in [brackets].
In the annotations, (fib n) really means that (memo-fib n) is called!
I just said "fib" to save space.

> (memo-fib 3)
"CALLED" memo-fib 3                          [user calls (fib 3)]
  "CALLED" lookup 3 (*table*)
  "RETURNED" lookup #f
  "CALLED" memo-fib 2                          [(fib 3) calls (fib 2)]
    "CALLED" lookup 2 (*table*)
    "RETURNED" lookup #f
    "CALLED" memo-fib 1                          [(fib 2) calls (fib 1)]
      "CALLED" lookup 1 (*table*)
      "RETURNED" lookup #f
      "CALLED" insert! 1 1 (*table*)
      "RETURNED" insert! ok
    "RETURNED" memo-fib 1                        [(fib 1) returns 1]
    "CALLED" memo-fib 0                          [(fib 2) calls (fib 0)]
      "CALLED" lookup 0 (*table* (1 . 1))
      "RETURNED" lookup #f
      "CALLED" insert! 0 0 (*table* (1 . 1))
      "RETURNED" insert! ok
    "RETURNED" memo-fib 0                        [(fib 0) returns 0]
    "CALLED" insert! 2 1 (*table* (0 . 0) (1 . 1))
    "RETURNED" insert! ok
  "RETURNED" memo-fib 1                        [(fib 2) returns 1]
  "CALLED" memo-fib 1                          [(fib 3) calls (fib 1) ****]
    "CALLED" lookup 1 (*table* (2 . 1) (0 . 0) (1 . 1))
    "RETURNED" lookup 1
  "RETURNED" memo-fib 1                        [(fib 1) returns 1]
  "CALLED" insert! 3 2 (*table* (2 . 1) (0 . 0) (1 . 1))
  "RETURNED" insert! ok
"RETURNED" memo-fib 2                        [(fib 3) returns 2]
2

The line marked **** above is the only call to memo-fib in this example in
which the memoization actually finds a previous value.  We are computing
(fib 1) for the second time, so memo-fib finds it in the table.

In general, calling memo-fib for some larger argument will result in two
recursive calls for each smaller argument value.  For example:

      fib 6  --->  fib 5,  fib 4
      fib 5  --->  fib 4,  fib 3
      fib 4  --->  fib 3,  fib 2

and so on.  (memo-fib 4) is evaluated once directly from (memo-fib 6) and once
from (memo-fib 5).  But only one of those actually requires any computation;
the other finds the value in the table.

This is why memo-fib takes Theta(n) time: it does about 2n recursive calls,
half of which are satisfied by values found in the table.

If we didn't use memoization, or if we defined memo-fib to be (memoize fib),
we would have had to compute (f 1) twice.  In this case there would only be
one duplicated computation, but the number grows exponentially; for (fib 4)
we have to compute (fib 2) twice and (fib 1) three times.

By the way, notice that if we try (memo-fib 3) a second time from the Scheme
prompt, we get a result immediately:

> (memo-fib 3)
"CALLED" memo-fib 3
"CALLED" lookup 3 (*table* (3 . 2) (2 . 1) (0 . 0) (1 . 1))
"RETURNED" lookup 2
"RETURNED" memo-fib 2
2



Vector exercises:

1.  VECTOR-APPEND is basically like VECTOR-CONS in the notes,
except that we need two loops, one for each source vector:

(define (vector-append vec1 vec2)
  (define (loop newvec vec n i)
    (if (>= n 0)
(begin (vector-set! newvec i (vector-ref vec n))
       (loop newvec vec (- n 1) (- i 1)))))
  (let ((result (make-vector (+ (vector-length vec1) (vector-length vec2)))))
    (loop result vec1 (- (vector-length vec1) 1) (- (vector-length vec1) 1))
    (loop result vec2 (- (vector-length vec2) 1) (- (vector-length result) 1))
    result))


2.  VECTOR-FILTER is tough because we have to do the filtering twice,
first to get the length of the desired result vector, then again to
fill in the slots:

(define (vector-filter pred vec)
  (define (get-length n)
    (cond ((< n 0) 0)
  ((pred (vector-ref vec n))
   (+ 1 (get-length (- n 1))))
  (else (get-length (- n 1)))))
  (define (loop newvec n i)
    (cond ((< n 0) newvec)
  ((pred (vector-ref vec n))
   (vector-set! newvec i (vector-ref vec n))
   (loop newvec (- n 1) (- i 1)))
  (else (loop newvec (- n 1) i))))
  (let ((newlen (get-length (- (vector-length vec) 1))))
    (loop (make-vector newlen) (- (vector-length vec) 1) (- newlen 1))))


3.  Bubble sort is notorious because nobody ever uses it in practice,
because it's slow, but it always appears in programming course
exercises, because the operation of swapping two neighboring elements
is relatively easy to write.

(a) Here's the program:

(define (bubble-sort! vec)
  (let ((len (vector-length vec)))
    (define (loop n)
      (define (bubble k)
(if (= k n)
    'one-pass-done
    (let ((left (vector-ref vec (- k 1)))
  (right (vector-ref vec k)))
      (if (> left right)
  (begin (vector-set! vec (- k 1) right)
(vector-set! vec k left)))
      (bubble (+ k 1)))))
      (if (< n 2)
  vec
  (begin (bubble 1)
(loop (- n 1)))))
    (loop len)))

(b) As the hint says, we start by proving that after calling (bubble 1) inside
the call to (loop n), element number n-1 is greater than any element to its
left.

(Bubble 1) reorders elements 0 and 1 so that vec[0] is less than or equal to
vec[1] (I'm using C/Java notation for elements of vectors), then reorders
elements 1 (the *new* element 1, which is the larger of the original first
two elements) and element 2 so that vec[1] is less than or equal to vec[2].
It continues, but let's stop here for the moment.  After those two steps,
the new vec[2] is at least as large as vec[1].  But the intermediate value
of vec[1] was larger than the new vec[0], so vec[2] must be the largest.

This might be clearer with a chart.  There are six possible original
orderings of the first three elements; here they are, with the ordering
after the 0/1 swap and the ordering after the 1/2 swap.  (To make the
table narrower, I've renamed VEC as V.  Also, I'm calling the three
values 0, 1, and 2; it doesn't matter what the actual values are, as
long as they are in the same order as a particular line in the table.)

original after 0/1 swap after 1/2 swap
-------------- -------------- --------------
v[0] v[1] v[2] v[0] v[1] v[2] v[0] v[1] v[2]
---- ---- ----  ---- ---- ----  ---- ---- ----

  0    1    2     0    1    2     0    1    2
  0    2    1     0    2    1     0    1    2
  1    0    2     0    1    2     0    1    2
  1    2    0     1    2    0     1    0    2
  2    0    1     0    2    1     0    1    2
  2    1    0     1    2    0     1    0    2

After the first swap, we have v[0] <= v[1].  After the second swap,
we have v[1] <= v[2].  But note that there is no guarantee about the
order of the final v[0] and v[1]!  All that's guaranteed is that
the largest of the three values is now in v[2].

Similarly, after the 2/3 swap, we know that vec[3] is the largest
of the first four values, because either the original vec[3] was
already largest, in which case there is no swap, or the value of
vec[2] just before the 2/3 swap is the largest of the original
vec[0] through vec[2], so it's the largest of vec[0] through vec[3]
and will rightly end up as the new vec[3].

Subprocedure BUBBLE calls itself recursively until k=n, which means
that vec[n-1] is the largest of the first n elements.  QED.

Now, if that's true about a single pass, then the first pass
"bubbles" the largest number to the end of the vector (this is why
it's called bubble sort), and then we call LOOP recursively to
sort the remaining elements.  The second pass gets vec[len-2] to
be the largest of the first len-1 elements, etc.  After LEN passes,
the entire vector is sorted.

This was a handwavy proof.  To make it rigorous, it'd be done by
mathematical induction -- two inductions, one for the swaps in a
single pass, and one for the multiple passes.

(c) It's Theta(N^2), for the usual reason: N passes, each of which
takes time Theta(N).


Extra for experts
-----------------

3.19 constant-space cycle? predicate  

Just to make sure you understand the issue, let me first do 3.18, which
asks us to write cycle? without imposing a constant-space requirement.
It's a lot like the correct version of count-pairs; it has to keep track
of which pairs we've seen already.

(define (cycle? lst)
  (define (iter lst pairlist)
    (cond ((not (pair? lst)) #f)
  ((memq lst pairlist) #t)
  (else (iter (cdr lst) (cons lst pairlist)))))
  (iter lst '()))

This is simpler than count-pairs because we only have to chase down pointers
in one direction (the cdr) instead of two, so it can be done iteratively.
I check (not (pair? lst)) rather than (null? lst) so that the program won't
blow up on a list structure like (a . b) by trying to take the cdr of b.

The trouble is that the list pairlist will grow to be the same size as the
argument list, if the latter doesn't contain a cycle.  What we need is to
find a way to keep the auxiliary list of already-seen pairs without using
up any extra space.

Here is the very cleverest possible solution:

(define (cycle? lst)
  (define (iter fast slow)
    (cond ((not (pair? fast)) #f)
  ((not (pair? (cdr fast))) #f)
  ((eq? fast slow) #t)
  (else (iter (cddr fast) (cdr slow))) ))
  (if (not (pair? lst))
      #f
      (iter (cdr lst) lst) ))

This solution runs in Theta(1) space and Theta(n) time.  We send two
pointers CDRing down the list at different speeds.  If the list is not a
cycle, the faster one will eventually hit the end of the list, and we'll
return false.  If the list is a cycle, the faster one will eventually
overtake the slower one, and we'll return true.  (You may think that this
will only work for odd-length cycles, or only for even-length cycles,
because in the opposite case the fast pointer will leapfrog over the slow
one, but if that happens the two pointers will become equal on the next
iteration.)

If you didn't come up with this solution, don't be upset; most folks don't.
This is a classic problem, and struggling with it is a sort of initiation
ritual in the Lisp community.  Here's a less clever solution that runs in
Theta(1) space but needs Theta(n^2) time.  It is like the first solution, the
one that uses an auxiliary pairlist, but the clever idea is to use the
argument list itself as the pairlist.  This can be done by clobbering its cdr
pointers temporarily.  It's important to make sure we put the list back
together again before we leave!  The idea is that at any time we will have
looked at some initial sublist of the argument, and we'll know for sure that
that part is cycle-free.  We keep the tested part and the untested part
separate by changing the cdr of the last tested pair to the empty list,
remembering the old cdr in the single extra pointer variable that this
algorithm requires.

(define (cycle? lst)
  (define (subq? x list)
    (cond ((null? list) #f)
  ((eq? x list) #t)
  (else (subq? x (cdr list)))))
  (define (iter lst pairlist pairlist-tail)
    (cond ((not (pair? lst))
   (set-cdr! pairlist-tail lst)
       #f)
  ((subq? lst pairlist)
   (set-cdr! pairlist-tail lst)
   #t)
  (else
   (let ((oldcdr (cdr lst)))
     (set-cdr! pairlist-tail lst)
     (set-cdr! lst '())
     (iter oldcdr pairlist lst) ))))
  (cond ((null? lst) #f)
(else (let ((oldcdr (cdr lst)))
(set-cdr! lst '())
(iter oldcdr lst lst)))))

Be wary of computing (cdr lst) before you've tested whether or not lst is
empty.


3.23  Double-ended queue

The only tricky part here is rear-delete-deque!.  All the other deque
operations can be performed in Theta(1) time using exactly the same structure
used for the queue in 3.3.2.  The trouble with rear-delete is that in order
to know where the new rear is, we have to be able to find the next-to-last
member of the queue.  In the 3.3.2 queue, the only way to do that is to cdr
down from the front, which takes Theta(n) time for an n-item queue.  To
avoid that, each item in the queue must point not only to the next item but
also to the previous item:

+---+---+
| | | --------------------------------------------+
+-|-+---+                                         |
  |                                               |
  V                                               V
+---+---+       +---+---+       +---+---+       +---+--/+
| | | --------->| | | --------->| | | --------->| | | / |
+-|-+---+       +-|-+---+       +-|-+---+       +-|-+/--+
  |   ^           |   ^           |   ^           |
  |   +-----+     |   +-----+     |   +-----+     |
  V         |     V         |     V         |     V
+--/+---+   |   +---+---+   |   +---+---+   |   +---+---+
| / | | |   +------ | | |   +------ | | |   +------ | | |
+/--+-|-+       +---+-|-+       +---+-|-+       +---+-|-+
      |               |               |               |
      V               V               V               V
      a               b               c               d


Whew!  The first pair, at the top of the diagram, is the deque header, just
like the queue header in 3.3.2.  The second row of four pairs is a regular
list representing the deque entries, again just like 3.3.2.  But instead of
each car in the second row pointing to a queue item, each car in this
second row points to another pair, whose car points to the previous element
on the second row and whose cdr points to the actual item.

;; data abstractions for deque members

;; we use front-ptr, rear-ptr, set-front-ptr!, and set-rear-ptr! from p. 263

(define deque-item cdar)
(define deque-fwd-ptr cdr)
(define deque-back-ptr caar)
(define set-deque-fwd-ptr! set-cdr!)
(define (set-deque-back-ptr! member new-ptr)
  (set-car! (car member) new-ptr))

;; Now the things we were asked to do:

(define (make-deque) (cons '() '()))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "front-deque called with empty queue")
      (deque-item (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "rear-deque called with empty queue")
      (deque-item (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-member (list (cons '() item))))
    (cond ((empty-deque? deque)
   (set-front-ptr! deque new-member)
   (set-rear-ptr! deque new-member)
   "done")
  (else
   (set-deque-fwd-ptr! new-member (front-ptr deque))
   (set-deque-back-ptr! (front-ptr deque) new-member)
   (set-front-ptr! deque new-member)
   "done"))))

(define (rear-insert-deque! deque item)
  (let ((new-member (list (cons '() item))))
    (cond ((empty-deque? deque)
   (set-front-ptr! deque new-member)
   (set-rear-ptr! deque new-member)
   "done")
  (else
   (set-deque-back-ptr! new-member (rear-ptr deque))
   (set-deque-fwd-ptr! (rear-ptr deque) new-member)
   (set-rear-ptr! deque new-member)
   "done"))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
(error "front-delete-deque! called with empty queue"))
((null? (deque-fwd-ptr (front-ptr deque)))
(set-front-ptr! deque '())
(set-rear-ptr! deque '())
"done")
(else
(set-deque-back-ptr! (deque-fwd-ptr (front-ptr deque)) '())
(set-front-ptr! deque (deque-fwd-ptr (front-ptr deque)))
"done")))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
(error "rear-delete-deque! called with empty queue"))
((null? (deque-back-ptr (rear-ptr deque)))
(set-front-ptr! deque '())
(set-rear-ptr! deque '())
"done")
(else
(set-deque-fwd-ptr! (deque-back-ptr (rear-ptr deque)) '())
(set-rear-ptr! deque (deque-back-ptr (rear-ptr deque)))
"done")))

I could also have gotten away with leaving garbage in the rear-ptr of
an emptied deque, but the ugliness involved outweighs the slight time
saving to my taste.  Notice an interesting property of the use of data
abstraction here: at the implementation level, set-deque-back-ptr! and
set-deque-fwd-ptr! are rather different, but once that difference is
abstracted away, rear-delete-deque! is exactly like front-delete-deque!
and ditto for the two insert procedures.

The reason these procedures return "done" instead of returning deque,
like the single-end queue procedures in the book, is that the deque is a
circular list structure, so if we tried to print it we'd get in trouble.
We should probably invent print-deque:

(define (print-deque deque)
  (define (sub member)
    (if (null? member)
'()
(cons (deque-item member)
      (sub (deque-fwd-ptr member)))))
  (sub (front-ptr deque)))

But I'd say it's a waste of time to cons up this printable list every time
we insert or delete something.


2.  cxr-name

This is a harder problem than its inverse function cxr-function!  We are
given a function as a black box, not knowing how it was defined; the only
way we can get any information about it is to invoke it on a cleverly
chosen argument.

We need three ideas here.  The first one is this:  Suppose we knew that we
were given either CAR or CDR as the argument.  We could determine which
of them by applying the mystery function to a pair with the word CAR as its
car, and the word CDR as its cdr:

(define (simple-cxr-name fn)
  (fn '(car . cdr)))

You might think to generalize this by building a sort of binary tree with
function names at the leaves:

(define (two-level-cxr-name fn)
  (fn '((caar . cdar) . (cadr . cddr))))

But there are two problems with this approach.  First, note that this
version *doesn't* work for CAR or CDR, only for functions with exactly
two CARs or CDRs composed.  Second, the argument function might be a
composition of *any* number of CARs and CDRs, so we'd need an infinitely
deep tree.

So the second idea we need is a way to attack the mystery function one
component at a time.  We'd like the first CAR or CDR applied to our argument
(that's the rightmost one, don't forget) to be the only one that affects the
result; once that first choice has been made, any CARs or CDRs applied to the
result shouldn't matter.  The clever idea is to make a pair whose CAR and CDR
both point to itself!  So any composition of CARs and CDRs of this pair will
still just be the same pair.

Actually we'll make two of these pairs, one for the CAR of our argument pair
and one for the CDR:

(define car-pair (cons '() '()))
(set-car! car-pair car-pair)
(set-cdr! car-pair car-pair)

(define cdr-pair (cons '() '()))
(set-car! cdr-pair cdr-pair)
(set-cdr! cdr-pair cdr-pair)

(define magic-argument (cons car-pair cdr-pair))

(define (rightmost-part fn)
  (if (eq? (fn magic-argument) car-pair)
      'car
      'cdr))

It's crucial that we're using EQ? rather than EQUAL? here, since car-pair
and cdr-pair are infinite (circular) lists.

Okay, we know the rightmost component.  How do we get all but the rightmost
component?  (Given that, we can recursively find the rightmost part of that,
etc.)  Our third clever idea is a more-magic argument that will give us
magic-argument whether we take its car or its cdr:

(define more-magic-arg (cons magic-argument magic-argument))

(define (next-to-rightmost-part fn)
  (if (eq? (fn more-magic-arg) car-pair)
      'car
      'cdr))

We're going to end up constructing a ladder of pairs whose car and cdr are
both the next pair down the ladder.  We also need a base case; if we apply
fn to magic-argument and get magic-argument itself, rather than car-pair
or cdr-pair, we've run out of composed CAR/CDR functions.

Here's how it all fits together:

(define (cxr-name fn)
  (word 'c (cxr-name-help fn magic-argument) 'r))

(define (cxr-name-help fn arg)
  (let ((result (fn arg)))
    (cond ((eq? result car-pair)
   (word (cxr-name-help fn (cons arg arg)) 'a))
  ((eq? result cdr-pair)
   (word (cxr-name-help fn (cons arg arg)) 'd))
  (else "")))) ; empty word if result is magic-argument
