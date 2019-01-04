;;; Faire les fonctions
;;; Les conditions
;;; Les boucles
;;; Wrapper l'ensemble des opérations
;;; Wrapper l'ensemble des comparaisons
;;; Ecrire un bloc d'instructions
;;; Opérations ne fonctionnent pas
;;; Appels systèmes de SPIM
;;; Conditions considérant une seule instruction


#lang racket/base

(require racket/match
         "mips.rkt"
         "ast.rkt")

(provide mips-data)
(provide mips-emit)
(provide comp)

;;;;; compilateur Python vers MIPS
;; la convention utilisée dans ce compilateur est
;; de toujours mettre la valeur calculée dans $v0 
;; et de placer dans $t9 la valeur des tests

(define accLoop 0) ;; accumulateur pour les boucles
(define accCond 0) ;; accumulateur pour les conditions
(define accStr  0) ;; accumulateur pour les chaînes de caractères

(provide comp)

;; Fonction réalisant les incrémentations pour nos accumulateurs pris sur 
;; https://stackoverflow.com
(define-syntax increment
  (syntax-rules ()
    ((_ x)   (begin (set! x (+ x 1)) x))
    ((_ x n) (begin (set! x (+ x n)) x))))

(define (comp ast env fp-sp) ;; le décalage entre sp et fp est fp - sp
  (match ast
    ((Null)
     ;; On représente Nil par l'adresse 0 (comme NULL en C)
     (comp (Const 0) env fp-sp))

    ((Const n)
     ;; Vérifier si il s'agit d'une chaîne. A continuer, mettre dans .data 
     ;;(if (string? n)
     ;; (increment accStr)
     ;; (hash-set mips-data (string-append "str_" (number->string accStr)) n))

     ;;; Matcher les valeurs obtenues par notre n.
     (list (match n
            ;; Si il s'agit d'une valeur booléenne, mettre soit 0 (false), soit 1 (true) afin
            ;; de respecter la convention 
            (boolean? (if (eq? n #t) 
                           (Li 'v0 1)
                           (Li 'v0 0)))
            ;; Autres: donc il s'agit "forcément" d'un entier
            (_ (Li 'v0 n)))))

    ((Data d)
     ;; Pointeur dans .data mis dans v0
     (list (La 'v0 (Lbl d))))

    ((Pair a b)
     ;; Paire (a . b)
     (append
      ;; d'abord on compile a pour avoir sa valeur dans v0 :
      (comp a env fp-sp)
      ;; ensuite on l'empile :
      (list (Addi 'sp 'sp -4)
            (Sw 'v0 (Mem 0 'sp)))
      ;; ensuite on compile b pour l'avoir dans v0 en
      ;; se rappelant qu'on est décalé par rapport à fp :
      (comp b env (- fp-sp 4))
      ;; ensuite on compile la paire :
      (list (Lw 't0 (Mem 0 'sp)) ;; on dépile a
            (Addi 'sp 'sp 4)
            (Move 't1 'v0) ;; on récupère b
            (Li 'a0 8) ;; on alloue 8 octets = 2 mots mémoire
            (Li 'v0 9)
            (Syscall)
            ;; l'adresse de la paire est déjà dans v0
            (Sw 't0 (Mem 0 'v0)) ;; dans le premier on écrit a
            (Sw 't1 (Mem 4 'v0))))) ;; dans le second on écrit b

    ((First p)
     ;; Premier élément de la paire p
     (append
      ;; on compile p pour avoir l'adresse de la paire dans v0 :
      (comp p env fp-sp)
      ;; on récupère le premier dans v0 :
      (list (Lw 'v0 (Mem 0 'v0)))))

    ((Second p)
     ;; Second élément de la paire p
     (append
      ;; on compile p pour avoir l'adresse de la paire dans v0 :
      (comp p env fp-sp)
      ;; on récupère le second élément dans v0
      (list (Lw 'v0 (Mem 4 'v0)))))

    ;; Operation en prenant en paramètre l'opérande et deux valeurs
    ((Op symbol v1 v2)
     (append

      (comp v1 env fp-sp)
      (list (Addi 'sp 'sp -4)
            (Sw 'v0 (Mem 0 'sp)))
        
      (comp v2 env (- fp-sp 4))
      
      (list (Lw 't0 (Mem 0 'sp)) ;; on dépile v1
            (Addi 'sp 'sp 4)
            (Move 't1 'v0) ;; on récupère v2
            (match symbol ;; On execute l'operation selon l'opérande
             ('Add (Add 'v0 't0 't1))
             ('Sub (Sub 'v0 't0 't1))
             ('Mul (Mul 'v0 't0 't1))
             ('Div (Div 'v0 't0 't1))
             ('Eq  (Seq 't9 't0 't1))
             ('Gt  (Sgt 't9 't0 't1))
             ('Lt  (Slt 't9 't0 't1))))))

    ((Call id args)
     (append
      (comp (Op id (car args) (car (cdr args))) env fp-sp)))

    ;; Test en prenant en paramètre le symbole de comparaison et deux valeurs
    ((Test symbol v1 v2)
     (append
      (comp v1 env fp-sp)
      (list (Addi 'sp 'sp -4)
            (Sw 'v0 (Mem 0 'sp)))

      (comp v2 env (- fp-sp 4))

      (list (Lw 't0 (Mem 0 'sp))
            (Addi 'sp 'sp 4)
            (Move 't1 'v0)
            (cond
             ((equal? symbol 'Eq)
              (Seq 't9 't0 't1))
             ((equal? symbol 'Gt)
              (Sgt 't9 't0 't1))
             ((equal? symbol 'Lt)
              (Slt 't9 't0 't1))
             (else
              (eprintf ("Error: Not a correct comparison instruction"))
              (exit 1))))))


    ;; Boucle qui compile d'abord le test puis va sur le label endloop 
    ;; si le test vaut 0 sinon on continue sur le label loop avec des
    ;; branchements vers le même label
    ((Loop test body)
     (increment accLoop)
     (append
      (comp test env fp-sp)
      (list (Addi 'sp 'sp -4)
            (Sw 'v0 (Mem 0 'sp)))
      (list (Label (string-append "loop_" (number->string accLoop)))
            (Beqz 't9 (string-append "endloop_" (number->string accLoop))))
      (comp body env (- fp-sp 4))
      (list (B (string-append "loop_" (number->string accLoop))))
      (list (Label (string-append "endloop_" (number->string accLoop))))))

    ;; Condition qui compile d'abord le test puis va sur le label then ou else 
    ;; Et une fois fini continue les instructions sur le label endif
    ((Cond test yes no)
     (increment accCond)
     (append
      ;; On compile d'abord le test 
      (comp test env fp-sp)
      (list (Addi 'sp 'sp -4)
            (Sw 'v0 (Mem 0 'sp)))
      (list (Bnez 't9 (string-append "then_" (number->string accCond))) ;; paramètre yes à utiliser
            (Beqz 't9 (string-append "else_" (number->string accCond))))

      (list (Label (string-append "then_" (number->string accCond))))
      (comp yes env (- fp-sp 4))
      (list (B (string-append "endif_" (number->string accCond))))

      (list (Label (string-append "else_" (number->string accCond))))
      (comp no env (- fp-sp 8))

      (list (Label (string-append "endif_" (number->string accCond))))))

    ;; Bloc d'instructions utilisé pour le corps des conditions, 
    ;; des boucles, ou des fonctions. A FAIRE!
    ((Block body)
     (apply append (map (lambda (expr)
             (comp expr env fp-sp))
          body)))

    ((Funblock body ret)
     (append
      (apply append (map (lambda (expr)
              (comp expr env fp-sp))
           body))
      (comp ret env fp-sp)))

    ((Func id closure)
     (append
      (list (Label id))
      (when (eq? id 'main)
       (list (Move 'fp 'sp)))
      (comp closure (hash-set env id (Mem fp-sp 'fp)) fp-sp)
      (list (Jr 'ra))))

    ((Closure rec? args body _)
     (append
      (comp body env fp-sp)))

    ((Let n v)
     ;; Variable locale : let n = v
     (append
      ;; on compile v pour avoir sa valeur dans v0 :
      (comp v (hash-set env n (Mem fp-sp 'fp)) fp-sp)
      ;; on empile la variable locale :
      (list (Addi 'sp 'sp -4)
            (Sw 'v0 (Mem 0 'sp)))))

      ;; à partir de là fp - sp a grandi de 4 :
  #|    (let ((fp-sp (- fp-sp 4)))
        ;; on compile e pour le mettre dans v0 :
        (comp e
              ;; en associant n à son adresse dans la pile par rapport
              ;; à fp, pour que cette adresse soit fixe
              (hash-set env n (Mem fp-sp 'fp))
              fp-sp)))) |#

    ((Var n)
     ;; Référence à une variable
     ;; on met la valeur de la variable dans v0 :
     (list (Lw 'v0 (hash-ref env n))))))



(define (mips-loc loc)
  (match loc
    ((Lbl l)   (format "~a" l))
    ((Mem b r) (format "~a($~a)" b r))))

(define (mips-emit instr)
  (match instr
    ((Move rd rs)     (printf "move $~a, $~a\n" rd rs))
    ((Li r i)         (printf "li $~a, ~a\n" r i))
    ((La r a)         (printf "la $~a, ~a\n" r (mips-loc a)))
    ((Addi rd rs i)   (printf "addi $~a, $~a, ~a\n" rd rs i))
    ((Add rd rs1 rs2) (printf "add $~a, $~a, $~a\n" rd rs1 rs2))
    ((Sub rd rs1 rs2) (printf "sub $~a, $~a, $~a\n" rd rs1 rs2))
    ((Mul rd rs1 rs2) (printf "mul $~a, $~a, $~a\n" rd rs1 rs2))
    ((Div rd rs1 rs2) (printf "div $~a, $~a, $~a\n" rd rs1 rs2))
    ((Lo rd)          (printf "mflo $~a\n" rd))
    ((Seq rd rs1 rs2) (printf "seq $~a, $~a, $~a\n" rd rs1 rs2))
    ((Sgt rd rs1 rs2) (printf "sgt $~a, $~a, $~a\n" rd rs1 rs2))
    ((Slt rd rs1 rs2) (printf "slt $~a, $~a, $~a\n" rd rs1 rs2))
    ((B l)            (printf "b ~a\n" l))
    ((Bnez rs l)      (printf "bnez $~a, ~a\n" rs l))
    ((Beqz rs l)      (printf "beqz $~a, ~a\n" rs l))
    ((Sw r loc)       (printf "sw $~a, ~a\n" r (mips-loc loc)))
    ((Lw r loc)       (printf "lw $~a, ~a\n" r (mips-loc loc)))
    ((Syscall)        (printf "syscall\n"))
    ((Jr r)           (printf "jr $~a\n" r))
    ((Label l)        (printf "\t~a:\n" l))))

(define (mips-data data)
  (printf ".data\n")
  (hash-for-each data
                 (lambda (k v)
                   (printf "~a: .asciiz ~s\n" k v)))
  (printf "\n.text\n.globl main\n"))

(mips-data (make-hash '((str_123 . "coucou") (nl . "\n"))))
