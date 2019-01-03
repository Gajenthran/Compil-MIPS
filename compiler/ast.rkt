#lang racket/base

(provide (all-defined-out))

(struct Let (n v)                  #:transparent)
(struct Var (n)                    #:transparent)
(struct Cond (test yes no)         #:transparent)
(struct Loop (test body)           #:transparent)
(struct Block (body)               #:transparent)
(struct Const (n)                  #:transparent)
(struct Nil ()                     #:transparent)
(struct Pair (a b)                 #:transparent)
(struct Data (l)                   #:transparent)
(struct First (p)                  #:transparent)
(struct Second (p)                 #:transparent)
(struct Op (symbol v1 v2)          #:transparent)
(struct Operand (symbol)           #:transparent)
(struct Test (symbol v1 v2)        #:transparent)
