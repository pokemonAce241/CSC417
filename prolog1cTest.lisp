#!/usr/bin/env clisp
;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab : 

#|

Help, my Prolog is broken. At the end of this file is a lisp
function called (test1) that runs when this file loads. e.g.

     clisp prolog1c.lisp

This should print out

      DONALD is the father of DEBBIE
      DONALD is the father of NANCY
      DEBBIE is the sibling of NANCY.
      NANCY is the sibling of DEBBIE.
      [1]
      ?x in chain1 matches to 1.
      ?x in chain2 matches to 1.
      ?x in chain4 matches to 1.

Sadly, it is does not cause some things are broken. Please fix!

First, some initial questions:

11111111111111111111111111111111111111111111111111
Part 1 is worth 1 mark. 0.5 marks for getting 7/10 
of the following right. 1 mark for getting 10/10

1. Write a file prolog1c.txt that answers the following questions.

1a. In LISP what is an association list?

Answer: It is a list of pairs of values.
Example: ((a . b)(b . c))

1b. What does the function `assoc` do:

      (assoc 'r '((a . b) (c . d) 
                  (r . x) (s . y) 
                  (r . z))) 

Answer: This finds the first value associated with r in the cons list.
Return Value: (r . x)

1c. What does the LISP 
[mapcan](http://jtra.cz/stuff/lisp/sclr/mapcan.html)
function do?  Gove am example of its use.

Answer: mapcan applies a function to elements in a list.  It's output is a string of the concatenated results.
Example: (mapcan (lambda (x) (list (+ x 10))) '(1 2 3 4 5))
Example Output: (11 12 13 14 15)

1d. Give a small example of using LISP hash tables to (1) crete a
hash table then (2) write something into that hash table then (3)
read that value back.

*

1e What does the LISP "sublis" function do? Give
an example.

Answer: sublis takes two arguments. Within one of the arguments, it replaces every instance of a certain value with another value.
Example: (sublis (mapcar (lambda (v) (cons v (gensym "?"))) (has-vars r)) r)
Example Explanation: This exaple replaces every symbol mathing r with a generated unique symbol.

1f. In Prolog, what is the role of the bindings variable "binds".

Answer: binds represents facts that are known.

1g. There seems to be a missing function. The examples shown below
use an `(= ?x ?x)` rule but there is no support code anywhere else
for `=`. So how does `(= ?x ?x)` work?

1h. What does "(gensym "?")" do?

Answer: gensym generates a unique symbol that is guaranteed to not already exist within the system.

1i. The following rules illustrates the need for variable renaming.
Why is such renaming required? What could go wrong (with the 
?x and ?y bindings) as our Prolog does inference over these two 
rules.

     (<- (child ?x ?y) (parent ?y ?x))
     (<- (daughter ?y ?x) (and (child ?y ?x) (female ?y)))

Answer: If you attempt to prove that ?y is a daughter of ?x, it would cause problems.
        That is because ?y is the parent in the test for (child ?x ?y), but ?y is the
        child in the test for (daughter ?y ?x).

1j. (HARD) The code for "prove" that handles conjunctions seem wrong.  Why
does the "and" clause in "prove" use "reverse"? Write a comment in
the "ands" function that explains why we needed that reverse.

22222222222222222222222222222222222222222222222222222
Part 2 is worth 1 mark

2a. The "(known x bindings)" function is missing. This is a function
that accepts a symbol "a" and list of dotted pairs.  While "a" can
be found in the car of any list then we set "a", to the cdr of that
list.  Once that stops recursing, we return the binding "a".
Otherwise, we return nil.  For example:

  (KNOWN '?X
    '((:?3044 . DEBBIE) (:?3045 . DONALD) 
      (?Y . :?3044) (?X . :?3045))) ==> DONALD

  (KNOWN '?Y
    '((:?3044 . DEBBIE) (:?3045 . DONALD) 
      (?Y . :?3044) (?X . :?3045))) ==> DEBBIE

  (KNOWN '?X
     '((:?3066 . 1) (:?3063 . 1) (:?3065 . 1) 
       (:?3064 . 1) (:?3061 . :?3064)
       (:?3062 . 1) (?X . :?3061))) ==> 1

2b. Another missing function is "(has-vars lst)" that
recursively explores "lst" looking for any symbol that starts
with "?" (see the "varp" function, below). Please implement:

   (HAS-VARS '(AND (PARENT ?X ?Y) (MALE ?X))) ==> (?Y ?X)

(Note that the order of the symbols in the output list does
not matter. But there can be **no** repeats).

33333333333333333333333333333333333333333333333333333
Part 3 is worth 1 mark

3a. The code "(do (show ?c))" crashes. Fix it such that "(do (show
?c))" prints the current binding to ?c, followed be a new line.
Hint: add an extra case into "prove".

3b. The prove function is missing anything that handles numeric
comparisons. So tests like (> ?c x) crashes. Please add code to
handle the maths functions ">=,>,<,<=".
Hint: this should be easy, once you've done (3).

3c. Please fix  definition of sibling such that a person cannot be
their own siblings. So the following output is wrong:

     DEBBIE is the sibling of DEBBIE.
     NANCY is the sibling of NANCY.

Hint: this a logic error, not an interpreter error. So you only
need to fix something inside `data0`.

|#

(defvar *rules* (make-hash-table))

;used to store a rule
(defmacro <- (con &optional ant)
  `(length
     (push (cons (cdr ',con) ',ant)
           (gethash (car ',con) *rules*))))

(defun data0 ()
  (clrhash *rules*)
  (<- (= ?x ?x))
  (<- (parent donald nancy))
  (<- (parent donald debbie))
  (<- (male donald))
  (<- (chain1 ?a ?b)
         (and (= ?a ?b)
              (= ?b ?c)
              (do (show ?c))
              (= ?c 1)))
  (<- (chain2 ?a ?b)
         (and (= ?a ?b)
              (= ?b ?c)
              (>  ?c 0.3)))
  (<- (chain3 ?a ?b)
         (and (= ?a ?b)
              (= ?b ?c)
              (> ?c 3)))
  (<- (chain4 ?a ?b)
         (and (= ?a ?b)
              (= ?b ?c)
              (not (> ?c 3))
              (= ?c 1)))
  (<- (father ?x ?y) 
      (and 
        (parent ?x ?y) 
        (male ?x)))
  (<- (sibling ?x ?y) 
      (and (parent ?z ?x)
           (parent ?z ?y)
           (not (= ?x ?y))))) ;This line was added to prevent a person from being their own sibling.


;--------- --------- --------- --------- --------- --------- ---------
; this is basically the main function of the program. it tests the rest of the program
; by performing various queries based on the data already established in the list data0
(defun test1 ()
  (data0)
  (query 
    (father ?x ?y)
    (format t "~A is the father of ~A~%" ?x ?y))
  (query 
    (sibling ?x ?y)
    (format t "~A is the sibling of ~A.~%" ?x ?y))
  (query 
    (chain1 ?x 1)
    (format t "?x in chain1 matches to ~A.~%" ?x))
  (query 
    (chain2 ?x 1)
    (format t "?x in chain2 matches to ~A.~%" ?x))
  (query 
    (chain3 ?x 1)
    (format t "?x in chain3 matches to ~A.~%" ?x))
  (query 
    (chain4 ?x 1)
    (format t "?x in chain4 matches to ~A.~%" ?x))
)
    
;--------- --------- --------- --------- --------- --------- ---------


(defun unify (x y &optional binds) 
  (cond 
    ((eql x y)        (values binds t))
    ((assoc x binds)  (unify (known x binds) y binds))
    ((assoc y binds)  (unify x (known y binds) binds))
    ((var? x)         (values (cons (cons x y) binds) t))
    ((var? y)         (values (cons (cons y x) binds) t))
    (t
      (when (and (consp x) (consp y))
        (multiple-value-bind (b2 yes) 
          (unify (car x) (car y) binds)
          (and yes (unify (cdr x) (cdr y) b2)))))))


(defun var? (x)
  (and (symbolp x) ;true if x is a symbol, nil if it isn't
       (eql (char (symbol-name x) 0) #\?)))

;; does no occur check cause crash?
;--------- --------- --------- --------- --------- --------- ---------
;query tests for a relationship between two entities
;sample uses:
#|
(query 
    (sibling ?x ?y)
    (format t "~A is the sibling of ~A.~%" ?x ?y))
  (query 
    (chain1 ?x 1)
    (format t "?x in chain1 matches to ~A.~%" ?x))
|#
(defmacro query (question &body body) 
  (let ((binds (gensym))) ;uses gensym to generate a unique key value and stores it as the variable binds
    `(dolist (,binds (prove ',question))
       (let ,(mapcar (lambda (v)
                         `(,v (known ',v ,binds)))
         (has-vars question))
   ,@body))))

;this method is for testing the relationship between two entities
(defun prove (expr &optional binds) ;Presumably expr is some kind of claim.
  (case (car expr)                  ;switches on the first item in expr
    (>    (evals       (cadr expr)            binds))
    (>=   (evals       (cadr expr)            binds))
    (<    (evals       (cadr expr)            binds))
    (<=   (evals       (cadr expr)            binds))
    (and  (ands        (reverse (cdr expr))   binds)) ;walks down a list in reverse order
    (or   (ors         (cdr  expr)            binds)) ;presumably, this checks if any of one or more claims are true
    (not  (negation    (cadr expr)            binds)) ;presumably, this checks if a given claim is not true
    (do   (evals       (cadr expr)            binds)) ;presumably, this prints a given binding
    (t    (prove1      (car  expr) (cdr expr) binds))))

;--------- --------- --------- --------- --------- --------- ---------
;this function supposedly checks that a number of facts are all true
(defun ands (goals binds)      
  (if (null goals)         ;true if goals is nil (empty)
      (list binds)                              ;makes a list out of binds
      (mapcan (lambda (b)                                ;lambda creates an anonymous function
                  (prove (car goals) b))                 ;attempts to prove the first goal using whatever "b" is
              (ands (cdr goals) binds))))                ;recurses with remainder of the goals and same binds

;this function supposedly tests that any of a number of claims is true
(defun ors(goals binds)                                  
  (mapcan (lambda (c) (prove c binds))
          goals))

;seems to make a list of binds if the goal/binds pair can't be proven,
;but doesn't seem to do anything with that. Supposedly, it returns nil
;if the recursive prove fails.
(defun negation (goal binds)
  (unless (prove goal binds)
    (list binds)))

;this function suppsedly prints a given binding
(defun evals (expr binds)
  " turns e.g. (print (list ?a ?b)) into
    (let ((?a x) ; where x is computed from (known ?a binds)
          (?b y)); where y is computed from (known ?b binds)
      (print ?a ?b))"
  (labels 
    ((local-vars ()
        (mapcar 
          (lambda (x) 
                 `(,x ',(known x binds))) 
             (has-vars expr))))
    (eval `(let ,(local-vars) 
              ,expr))
    (list binds)))

;
(defun prove1 (pred args binds)
  (mapcan 
    (lambda (r)
        (multiple-value-bind (b2 yes) 
          (unify args (car r) 
                 binds)
          (when yes
            (if (cdr r)  
              (prove (cdr r) b2) 
              (list b2)))))
    (mapcar #'renames
            (gethash pred *rules*))))

;--------- --------- --------- --------- --------- --------- ---------
; renames variables with a generated symbol
(defun renames (r)
  (sublis (mapcar (lambda (v) (cons v (gensym "?")))
                  (has-vars r))
          r))


;------------------------------------------------------------------r
#|
2a. The "(known x bindings)" function is missing. This is a function
that accepts a symbol "a" and list of dotted pairs.  While "a" can
be found in the car of any list then we set "a", to the cdr of that
list.  Once that stops recursing, we return the binding "a".
Otherwise, we return nil.  For example:

  (KNOWN '?X
    '((:?3044 . DEBBIE) (:?3045 . DONALD) 
      (?Y . :?3044) (?X . :?3045))) ==> DONALD

  (KNOWN '?Y
    '((:?3044 . DEBBIE) (:?3045 . DONALD) 
      (?Y . :?3044) (?X . :?3045))) ==> DEBBIE

  (KNOWN '?X
     '((:?3066 . 1) (:?3063 . 1) (:?3065 . 1) 
       (:?3064 . 1) (:?3061 . :?3064)
       (:?3062 . 1) (?X . :?3061))) ==> 1
|#
(defun knownHelper (x binds)
)

(defun known (x binds) ;finds the meaning of x in binds. Can x have multiple meanings
)

#|
2b. Another missing function is "(has-vars lst)" that
recursively explores "lst" looking for any symbol that starts
with "?" (see the "varp" function, below). Please implement:

   (HAS-VARS '(AND (PARENT ?X ?Y) (MALE ?X))) ==> (?Y ?X)

(Note that the order of the symbols in the output list does
not matter. But there can be **no** repeats).
|#
(defun has-varsHelper2 (x f)
  "generic visitor"
  (if (atom x)
    (funcall f x)
    (dolist (y x)
      (has-varsHelper2 y f)
    )
  )
)
(defun has-varsHelper1 (x p  &optional out )
 "visit, collecting the result"
 (has-varsHelper2 x (lambda (y)
   (if (funcall p y)
     (pushnew y out)
   )
 ))
 out
)
(defun has-vars (lst)
  (has-varsHelper1 lst #'var?)
)


#|
3a. The code "(do (show ?c))" crashes. Fix it such that "(do (show
?c))" prints the current binding to ?c, followed be a new line.
Hint: add an extra case into "prove".
|#
(defun show (var)
  (print var)
)
;---------------------------------------------------

(test1) ; runs the test for the program (basically calling main)
