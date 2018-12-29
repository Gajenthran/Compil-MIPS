#lang racket/base

(provide (all-defined-out))

;;;;; AST
(struct Nil ())
(struct Pair (a b))
(struct Num (n))
(struct Data (l))
(struct First (p))
(struct Second (p))
(struct Let (n v e))
(struct Var (n))
(struct Op (oper v1 v2))
(struct Test (symbol v1 v2))
(struct Cond (test yes no))
(struct Loop (test body))
(struct Block (label body))