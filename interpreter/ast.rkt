#lang racket/base

(provide (all-defined-out))


;;; Syntactic structures representing the parsed syntax
;;; ---------------------------------------------------

;;; Definition of variables
;; type id "=" expr
(struct Pvardef (id expr type pos)          #:transparent)

;;; Definition of variables
;; "let" type id "=" expr
(struct Pvar    (id expr pos)				#:transparent)

;;; Definition of functions
;; ( "rec" )? type-ret id "(" args ":" type-args ")" body
(struct Pfundef (rec id args body type pos) #:transparent)

;;; Identifier
;; id
(struct Pident  (id pos)                    #:transparent)

;;; Function call
;; id args
(struct Pcall   (id args pos)               #:transparent)

;;; Conditional branching
;; "if" test "then" yes "else" no
(struct Pcond   (test yes no pos)           #:transparent)

;;; while iter
;; "while" iter body
(struct Piter   (test body pos)             #:transparent)

;;; Function block
;; "{" expr ( ";" expr )* return sexpr "}" 
(struct Pfunblock (exprs ret pos)           #:transparent)

;;; Block
;; "{" expr ( ";" expr )* "}"
(struct Pblock  (exprs pos)                 #:transparent) ;;; TODO

;;; Constant values
;; value
(struct Pconst  (type value pos)            #:transparent)


;;; Syntactic structures for our internal representation (AST)
;;; ----------------------------------------------------------

;;; Definition
;; Binds <id> to <def> in the subsequent expressions.
(struct Let     (id def)                    #:transparent)

;;; Closure
;; Function that takes <args> as arguments and returns the value of the
;; expression <body> where the arguments are binded to their call-specific
;; values and other free variables are looked up in its lexical environment
;; <env>.
(struct Closure (rec? args body env)        #:transparent)

;;; Variable
;; References a variable by its name <id>.
(struct Var     (id)                        #:transparent)

;;; Function call
;; Call the function <id> which should reference a closure and pass arguments
;; <args> to it.
(struct Call    (id args)                   #:transparent)

;;; Conditional branching
;; Evaluates to <yes> if <test> evaluates to true, otherwise evaluates to <no>.
(struct Cond    (test yes no)               #:transparent)

(struct Iter    (iter body)                 #:transparent)

;;; Block
;; Evaluates to the value of last expression in the list <exprs>. Previous
;; expressions are evaluated for their side-effect (binding, printing, …).
(struct Block   (exprs)                            #:transparent)

(struct Funblock (exprs ret)                     #:transparent)

;;; Constant value
;; Evaluates to the value of <value>.
(struct Const   (value)                     #:transparent)


;;; Types
;;; -----

;;; Numbers
(define Int 'int)

;;; Strings
(define Str 'str)

;;; Booleans
(define Bool 'bool)

;;; Nil / the empty list
(define Nil 'nil)

;;; Anything
(define Any 'any)

;;; List of <t>
(struct Lst (t)                             #:transparent)

;;; Function that takes <args> and returns <ret>
(struct Fun (ret args)                      #:transparent)
