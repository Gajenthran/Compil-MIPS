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


(struct Let (n v)                      #:transparent)
(struct Func (id def)                  #:transparent)
(struct Var (n)                        #:transparent)
(struct Cond (test yes no)             #:transparent)
(struct Loop (test body)               #:transparent)
(struct Block (body)                   #:transparent)
(struct Const (n)                      #:transparent)
(struct Null ()                        #:transparent)
(struct Pair (a b)                     #:transparent)
(struct Data (l)                       #:transparent)
(struct First (p)                      #:transparent)
(struct Second (p)                     #:transparent)
(struct Op (symbol v1 v2)              #:transparent)
(struct Operand (symbol)           	   #:transparent)
(struct Test (symbol v1 v2)            #:transparent)
(struct Funblock (exprs ret)           #:transparent)
(struct Call    (id args)              #:transparent)
;;(struct Func (id closure)		       #:transparent)
(struct Closure (rec? args body env)   #:transparent)


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
