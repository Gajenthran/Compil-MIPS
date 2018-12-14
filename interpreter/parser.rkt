#lang racket/base

(require parser-tools/yacc
         "lexer.rkt"
         "ast.rkt")

(provide liec-parser)


(require (for-syntax racket/base))
(require compatibility/defmacro)
(defmacro sp n
  (string->symbol (string-append "$" (number->string (car n)) "-start-pos")))

(define parse
  (parser
   (src-pos)
   (tokens keywords operators punctuations atoms)
   (start prog)
   (end Leof)

   (grammar

    ;; (ftype
    ;;  ((type Larrow argtypes) (Fun $1 $3)))

    (prog
     ((definition Lsc prog) (cons $1 $3))
     ((definition)          (list $1)))

    (definition
      ;; ((Llet Lident fargs Lcol ftype Lassign expr)           (Pfundef #f $2 $3 $7 $5 (sp 2)))
      ;; ((Llet type Lident Lassign expr)                       (Pvardef $3 $5 $2 (sp 2)))
      ;; ((Llet Lrec Lident fargs Lcol ftype Lassign expr)      (Pfundef #t $3 $4 $8 $6 (sp 3)))
      ((type Lident Lassign expr)                               (Pvardef $2 $4 $1 (sp 2)))
      ((type Lident fargs Lcol argtypes Lassign expr)           (Pfundef #f $2 $3 $7 (Fun $1 $5) (sp 2)))
      ((Lrec type Lident fargs Lcol argtypes Lassign expr)      (Pfundef #t $3 $4 $8 (Fun $2 $6) (sp 3)))
      ((Lident Lassign expr)                                    (Pvar    $1 $3 (sp 2))))

    (type
     ((type Llist) (Lst $1))
     ((Ltype)      $1))

    (argtypes
     ((type Lcom argtypes)              (cons $1 $3))
     ((type)                            (list $1)))
     ;; ((Lopar ftype Lcpar Lcom argtypes) (cons $2 $5))
     ;;((Lopar ftype Lcpar)               (list $2)))

    (fargs
     ((Lident Lcom fargs) (cons (Pident $1 (sp 1)) $3))
     ((Lident)            (list (Pident $1 (sp 1))))
     ((Lnil)              (list (Pident 'nil (sp 1)))))

    (expr
     ((definition)          $1)
     ((atom)                $1)
     ((funcall)             $1)
     ((operation)           $1)
     ((test)                $1)
     ((iter)                $1)
     ((Lopar expr Lcpar)    $2)
     ((Locbra exprs Lccbra) (Pblock $2 (sp 1))))

    (exprs
     ((expr Lsc exprs) (cons $1 $3))
     ((expr)           (list $1)))

    
    (iter
      ((Lwhile expr Lthen expr) (Piter $2 $4 (sp 1))))

    (test
     ((Lif expr Lthen expr Lelse expr) (Pcond $2 $4 $6 (sp 1)))) ;; TODO

    (funcall
     ((Lident args) (Pcall $1 $2 (sp 1))))

    (args
     ((sexpr args) (cons $1 $2))
     ((sexpr)      (list $1)))

    (sexpr ;; single-expr
     ((atom)             $1)
     ((operation)        $1)
     ((Lopar expr Lcpar) $2))

    (operation
     ((sexpr Ladd sexpr) (Pcall '+ (list $1 $3) (sp 1)))
     ((sexpr Lsub sexpr) (Pcall '- (list $1 $3) (sp 1)))
     ((sexpr Lmul sexpr) (Pcall '* (list $1 $3) (sp 1)))
     ((sexpr Ldiv sexpr) (Pcall '/ (list $1 $3) (sp 1)))
     ((sexpr Lmod sexpr) (Pcall '% (list $1 $3) (sp 1)))

     ((sexpr Leq sexpr)  (Pcall '== (list $1 $3) (sp 1)))
     ((sexpr Lneq sexpr) (Pcall '!= (list $1 $3) (sp 1)))
     ((sexpr Llt sexpr)  (Pcall '<  (list $1 $3) (sp 1)))
     ((sexpr Lgt sexpr)  (Pcall '>  (list $1 $3) (sp 1)))
     ((sexpr Llte sexpr) (Pcall '<= (list $1 $3) (sp 1)))
     ((sexpr Lgte sexpr) (Pcall '>= (list $1 $3) (sp 1)))

     ((sexpr Land sexpr) (Pcall 'and (list $1 $3) (sp 1)))
     ((sexpr Lor sexpr)  (Pcall 'or  (list $1 $3) (sp 1)))
     ((Lnot sexpr)       (Pcall 'not (list $2) (sp 1)))

     ((sexpr Lcc sexpr)  (Pcall 'cons (list $1 $3) (sp 1))))

    (atom
     ((Lnil)   (Pconst 'nil '() (sp 1)))
     ((Lbool)  (Pconst 'bool $1 (sp 1)))
     ((Lnum)   (Pconst 'int $1 (sp 1)))
     ((Lstr)   (Pconst 'str $1 (sp 1)))
     ((Lident) (Pident $1 (sp 1)))
     ((Lobra elem Lcbra) $2))

    (elem
     ((expr Lcom elem) (Pcall 'cons (list $1 $3) (sp 1)))
     ((expr)           (Pcall 'cons (list $1 (Pconst 'nil '() #f)) (sp 1)))
     (()              (Pconst 'nil '() #f)))

   )

   (precs (left Lcc)

          (left Lor)
          (left Lxor)
          (left Land)
          (right Lnot)

          (left Leq)
          (left Lneq)
          (left Llt)
          (left Lgt)
          (left Llte)
          (left Lgte)

          (left Lmod)
          (left Ladd)
          (left Lsub)
          (left Lmul)
          (left Ldiv))

   (debug "yacc.dbg")
   (error
    (lambda (ok? name value s-pos e-pos)
      (eprintf "Parser: ~a: ~a~a on line ~a col ~a.\n"
               (substring (symbol->string name) 1)
               (if ok? "syntax error" "unexpected token")
               (if value (format " near '~a'" value) "")
               (position-line s-pos)
               (position-col s-pos))
      (exit 1)))))

(define (liec-parser in)
  (parse (lambda () (liec-lexer in))))
