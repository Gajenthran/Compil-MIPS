#lang racket/base

(require "ast.rkt")

(provide (all-defined-out))

;;; Types of the functions in the standard library
(define *stdlib-types*
  (make-immutable-hash
   (list
    (cons 'cons  (Fun (Lst Any) (list Any (Lst Any))))
    (cons 'hd    (Fun Any (list (Lst Any))))
    (cons 'tl    (Fun (Lst Any) (list (Lst Any))))
    (cons 'nullp (Fun Bool (list (Lst Any))))
    
    (cons 'Add    (Fun Int (list Int Int)))
    (cons 'Sub    (Fun Int (list Int Int)))
    (cons 'Mult   (Fun Int (list Int Int)))
    (cons 'Div    (Fun Int (list Int Int)))
    (cons 'Modulo (Fun Int (list Int Int)))
    (cons 'zerop  (Fun Bool (list Int)))
    
    (cons 'Eq (Fun Bool (list Int Int)))
    (cons 'Neq (Fun Bool (list Int Int)))
    (cons 'Lt  (Fun Bool (list Int Int)))
    (cons 'Gt  (Fun Bool (list Int Int)))
    (cons '<= (Fun Bool (list Int Int)))
    (cons '>= (Fun Bool (list Int Int)))
    
    (cons 'and (Fun Bool (list Bool Bool)))
    (cons 'or  (Fun Bool (list Bool Bool)))
    (cons 'not (Fun Bool (list Bool)))
    
    (cons 'print_num  (Fun Nil (list Int)))
    (cons 'print_bool (Fun Nil (list Bool)))
    (cons 'print_str  (Fun Nil (list Str)))
    (cons 'print_lst  (Fun Nil (list Any)))
    
    (cons 'strcat (Fun Str (list Str Str)))
    (cons 'atoi   (Fun Int (list Str))))))

;;; Values of the function in te standard library
;;; A retirer ou adapter avec le compilateur
(define *stdlib*
  (make-immutable-hash
   (list
    (cons 'cons  cons)
    (cons 'hd    car)
    (cons 'tl    cdr)
    (cons 'nullp null?)
    
    (cons 'Add     +)
    (cons 'Sub     -)
    (cons 'Mult     *)
    (cons 'Div     /)
    (cons 'Modulo     modulo)
    (cons 'zerop zero?)
    
    (cons 'Eq =)
    (cons 'Neq (lambda (a b) (not (= a b))))
    (cons 'Lt  <)
    (cons 'Gt  >)
    (cons '<= <=)
    (cons '>= >=)
    
    (cons 'and (lambda (a b) (and a b)))
    (cons 'or  (lambda (a b) (or a b)))
    (cons '! not)
    
    (cons 'print_num  displayln)
    (cons 'print_bool displayln)
    (cons 'print_str  displayln)
    (cons 'print_lst  displayln)
    (cons 'printf     printf)
    
    (cons 'strcat string-append)      ;; TODO : string.h functions
    (cons 'atoi   string->number))))
