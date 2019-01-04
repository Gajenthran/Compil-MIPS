#lang racket/base

(require "parser.rkt"
         "semantics.rkt"
         "compiler-mips.rkt"
         "stdlib.rkt"
         "mips.rkt"
         "ast.rkt"
         "parser.rkt"
         "stdlib.rkt")

;;; Analyses du programme et compilation de ce dernier
;;; A mettre dans un autre fichier
(define argv (current-command-line-arguments))
(cond
  ((>= (vector-length argv) 1)
   (define in (open-input-file (vector-ref argv 0)))
   (port-count-lines! in)
   (define parsed (liec-parser in))
   (close-input-port in)

   ;;(define prog (check-exprs parsed *stdlib-types* Nil))
   (define prog (liec-check parsed)); *stdlib-types* Nil))
   (displayln prog)
   ;;(mips-data (make-hash '((str_123 . "coucou") (nl . "\n"))))
   (for-each mips-emit
          (append
           ;; On initialise notre environnement local :
;;           (list (Move 'fp 'sp))

           ;; On compile une expression :
           (comp (Block prog)
                 ;;(Func 'toto (Closure #f (Const 99) (Block (list (Let 'nbo (Const 11)) (Const 24))) (make-immutable-hash)))
                 ;;(Op 'Add (Op 'Mul (Const 4) (Const 1)) (Const 5))
                 ;;(Block (list (Let 'bobo (Const 18)) (Null)))
                 ;;(Block (list (Const 18) (Const 24)))
                 ;;(Op 'Div (Const 14) (Const 11))
                 ;;(Cond (Test 'Gt (Const 14) (Const 11)) (Let 'nbo (Const 93)) (Const 28))
                 ;;(Loop (Test 'Eq (Const 92) (Const 42)) (Const 33))
                 ;;(Let 'a (Const 18))
                 ;; ((Test 'Eq (Num 32) (Num 42))
                 ;;       (Let 'b (Num 18))
                 ;;       (Let 'b (Num 0))))

                 ;; avec un environnement vide :
                 (make-immutable-hash)
                 ;; et fp-sp = 0 (vu que fp = sp à ce moment là) :
                 0)


           ;; On affiche le résultat, qui est dans v0
           (list (Move 't5 'v0)
                 (Li 'v0 1)
                 (Move 'a0 't5)
                 (Syscall)
                 ;;(Move 'a0 'v0)
                 ;;(Li 'v0 4) ;; 4 pour print_string qui est le type du résultat
                 ;;(Syscall)
                 ;; affichage retour à la ligne :
                 ;;(La 'a0 (Lbl 'nl))
                 ;;(Syscall)
                 ;;(Li 'v0 4)
                 ;; main return 0
                 (Li 'v0 0)
                 (Jr 'ra)))))
  
  (else
   (eprintf "Usage: racket liec.rkt <source.liec>\n")
   (exit 1)))