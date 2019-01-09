#lang racket/base

(require "parser.rkt"
         "semantics.rkt"
         "eval.rkt"
         "ast.rkt"
         "stdlib.rkt")

(define argv (current-command-line-arguments))
(cond
  ((>= (vector-length argv) 1)
   (define in (open-input-file (vector-ref argv 0)))
   (port-count-lines! in)
   (define parsed (liec-parser in))
   (close-input-port in)
   (printf "Parsing ok.\n")

   (define prog (liec-check parsed))
   (printf "Typing ok.\n")

   (define ret (liec-eval prog))
   (newline)
   (exit ret))

  
  (else
   (eprintf "Usage: racket liec.rkt <source.liec>\n")
   (exit 1)))
