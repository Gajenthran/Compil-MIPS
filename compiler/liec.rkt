#lang racket/base

(require "parser.rkt"
         "semantics.rkt"
         "eval.rkt"
         "astint.rkt"
         "stdlib.rkt"
         "compiler-mips.rkt")

(define argv (current-command-line-arguments))
(cond
  ((>= (vector-length argv) 1)
   (define in (open-input-file (vector-ref argv 0)))
   (port-count-lines! in)
   (define parsed (liec-parser in))
   (close-input-port in)

   (define prog (check-exprs parsed *stdlib-types* Nil))

   (define ret (comp (Let 'Toto (Const 18)) (make-immutable-hash) 0))
   (displayln ret))
  
  (else
   (eprintf "Usage: racket liec.rkt <source.liec>\n")
   (exit 1)))
