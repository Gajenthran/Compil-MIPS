#lang racket/base

(require "ast.rkt")

(provide (all-defined-out))

;;; Types of the functions in the standard library
(define *stdlib-types*
  (make-immutable-hash
   (list
    (cons 'cons (Fun (Lst Any) (list Any (Lst Any))))
    (cons 'hd (Fun Any (list (Lst Any))))
    (cons 'tl (Fun (Lst Any) (list (Lst Any))))
    (cons 'nullp (Fun Bool (list (Lst Any))))
    
    (cons '+ (Fun Num (list Num Num)))
    (cons '- (Fun Num (list Num Num)))
    (cons '* (Fun Num (list Num Num)))
    (cons '/ (Fun Num (list Num Num)))
    (cons '% (Fun Num (list Num Num)))
    (cons 'zerop (Fun Bool (list Num)))
    
    (cons '== (Fun Bool (list Num Num)))
    (cons '!= (Fun Bool (list Num Num)))
    (cons '< (Fun Bool (list Num Num)))
    (cons '> (Fun Bool (list Num Num)))
    (cons '<= (Fun Bool (list Num Num)))
    (cons '>= (Fun Bool (list Num Num)))
    
    (cons 'and (Fun Bool (list Bool Bool)))
    (cons 'or (Fun Bool (list Bool Bool)))
    (cons 'not (Fun Bool (list Bool)))
    
    (cons 'print_num (Fun Nil (list Num)))
    (cons 'print_bool (Fun Nil (list Bool)))
    (cons 'print_str (Fun Nil (list Str)))
    
    (cons 'str_append (Fun Str (list Str Str))))))

;;; Values of the function in te standard library
(define *stdlib*
  (make-immutable-hash
   (list
    (cons 'cons cons)
    (cons 'hd car)
    (cons 'tl cdr)
    (cons 'nullp null?)
    
    (cons '+ +)
    (cons '- -)
    (cons '* *)
    (cons '/ /)
    (cons '% modulo)
    (cons 'zerop zero?)
    
    (cons '== =)
    (cons '!= (lambda (a b) (not (= a b))))
    (cons '< <)
    (cons '> >)
    (cons '<= <=)
    (cons '>= >=)
    
    (cons 'and (lambda (a b) (and a b)))
    (cons 'or (lambda (a b) (or a b)))
    (cons 'not not)
    
    (cons 'print_num displayln)
    (cons 'print_bool displayln)
    (cons 'print_str displayln)
    
    (cons 'str_append string-append))))
