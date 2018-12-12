#lang racket/base

(require racket/match
         "ast.rkt"
         "stdlib.rkt")

(provide liec-eval)

;;; Evaluating a program consists in evaluating the list of definitions
;;; and then calling its main function
(define (liec-eval prog)
  (let ((argv (current-command-line-arguments))
        ;; evaluating the list of definitions with *stdlib* as initial env
        (env (cdr (eval-exprs prog *stdlib*))))
         ;; calling the main function with argc - 1 (as argv[0] is our program)
    (car (eval-expr (Call 'main (list (Const (- (vector-length argv) 1))
                                      ;; and the list of strings corresponding
                                      ;; to argv
                                      (foldl (lambda (arg acc)
                                               (Call 'cons (list (Const arg)
                                                                 acc)))
                                             (Const '())
                                             (reverse (cdr (vector->list argv))))))
                    env))))

;;; Evaluation a list of expressions means evaluating them one by one
;;; using the updated environment and returning the value of the last one
(define (eval-exprs exprs env)
  (foldl (lambda (expr acc)
           (eval-expr expr (cdr acc)))
         (cons #f env)
         exprs))


;;; Evaluating an expression is quite straightforward
(define (eval-expr expr env)
  (match expr

    ;; Binding a variable evaluates to nothing but adds it to the environment
    ((Let id def)
     (cons #f
           (hash-set env id (car (eval-expr def env)))))

    ;; Closure definition amounts to capturing its lexical environment
    ((Closure rec? args body _)
     (cons (Closure rec? args body env)
           env))

    ;; Variables values are in the environment (we know they exist from typing)
    ((Var id)
     (cons (hash-ref env id)
           env))

    ;; Calling a function depends on its type: is it a user-defined closure or
    ;; a native function from the stdlib?
    ((Call id args)
     (cons (let ((f (hash-ref env id)))
             (if (Closure? f)
                 ;; if it is a user-defined closure, evaluates its body
                 (car (eval-expr (Closure-body f)
                                 ;; in the lexical environment of the closure
                                 ;; augmented with its arguments bindings.
                                 (for/fold
                                     ;; initial e is the lexical env
                                     ;; plus the closure itself if it is recursive
                                     ((e (if (Closure-rec? f)
                                             (hash-set (Closure-env f) id (hash-ref env id))
                                             (Closure-env f))))

                                     ;; for each formal args fa
                                     ((fa (Closure-args f))
                                      ;; for each args a
                                      (a args))

                                   ;; add a binding from fa to a evaluated
                                   (hash-set e fa (car (eval-expr a env))))))
                 ;; otherwise call the native function.
                 (apply f (map (lambda (a)
                                 (car (eval-expr a env)))
                               args))))
           env))

    ;; Condition evaluated using native condition
    ((Cond test yes no)
     (cons (car (if (car (eval-expr test env))
                    (eval-expr yes env)
                    (eval-expr no env)))
           env))

    ;; Block are evaluated as list of expressions
    ((Block exprs)
     (eval-exprs exprs env))

    ;; Constant are their own value
    ((Const value)
     (cons value
           env))
    ))
