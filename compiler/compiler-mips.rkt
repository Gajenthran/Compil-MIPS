;;; Faire les fonctions
;;; Les conditions
;;; Les boucles
;;; Tous les signes à mettres


#lang racket/base

(require racket/match
         racket/list
         "mips.rkt"
         "ast.rkt")

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

;; Fonction réalisant les incrémentations pour nos accumulateurs récupérée 
;; sur https://stackoverflow.com
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

     ;;; Vérifier la valeur obtenue dans notre n.
     (list
      (list 
       (cond
         ;; Si il s'agit d'une valeur booléenne, mettre soit 0 (false), soit 1 (true) afin
         ;; de respecter la convention 
         ((boolean? n)
          (if (eq? n #t) 
           (Li 'v0 10)
           (Li 'v0 100)))
          ;; Autres: donc il s'agit "forcément" d'un entier
          (else 
           (Li 'v0 n))))
      env))

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

    ;; Opération en prenant en paramètre l'opérande et deux valeurs.
    ;; Il s'agit d'instructions arithmétiques mais également de comparaisons
    ((Op symbol v1 v2)
     ;; compiler la première valeur et la deuxième valeur
     (define cv1 (comp v1 env fp-sp))
     (define cv2 (comp v2 env fp-sp))
     (list
      (append

       (first cv1)
       (list (Addi 'sp 'sp -4)
             (Sw 'v0 (Mem 0 'sp)))
         
       (first cv2)
       
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
              ('Lt  (Slt 't9 't0 't1)))))
      env))


    ;; Appels systèmes. Pour l'instant, seulement pour l'affichage
    ((Systcall id value)
     (define cval (comp value env fp-sp))
     (list
      (append
       (first cval)
       (match id
        ('print_num (list (Move 'a0 'v0) (Li 'v0 1) (Syscall)))
        ('print_str (list (La 'a0 Lbl value) (Li 'v0 4) (Syscall)))))
      env))

    ;; Appel de fonction qui peuvent être des fonctions déclarées par l'utilisateur (pas encore
    ;; implémenté) ou des fonctions natives (appel systèmes ou opérations)
    ((Call id args)
     ;; Expression compilée qui va soit être un appel système, soit une opération
     (let ((ce 
      (if (eq? id (or 'print_num 'print_str))
        (comp (Systcall id (car args)) env fp-sp)
        (comp (Op id (car args) (car (cdr args))) env fp-sp))))
     (list
      (append (first ce))
      second ce)))

    ;; Boucle qui compile d'abord le test puis va sur le label endloop 
    ;; si le test vaut 0 sinon on continue sur le label loop avec des
    ;; branchements vers le même label
    ((Loop test body)
     (increment accLoop)
     (define ctest (comp test env fp-sp))
     (define cbody (comp body env (- fp-sp 4)))
     (list
      (append
       ;; Compilation de l'expression pour le test
       (first ctest)
       (list (Addi 'sp 'sp -4)
             (Sw 'v0 (Mem 0 'sp)))
       (list (Label (string-append "loop_" (number->string accLoop)))
             (Beqz 't9 (string-append "endloop_" (number->string accLoop))))
       ;; Compilation de l'expression pour le corps de la boucle
       (first cbody)
       (list (B (string-append "loop_" (number->string accLoop))))
       (list (Label (string-append "endloop_" (number->string accLoop)))))
      (second cbody)))

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


    ;;; Déclaration d'une fonction composé de id (nom de la fonction) et 
    ;;; de sa clôture
    ((Func id closure)
     (define ce (comp closure (hash-set env id (Mem fp-sp 'fp)) fp-sp))
     (list
      (append
       (if (eq? id 'main)
        (list (Label '_main)) ;; Main déjà existant
        (list (Label id)))
       (first ce)
       (list (Jr 'ra)))
      (second ce)))

    ;; Clôture... A faire !
    ((Closure rec? args body _)
     ;; Expression compilée avec 2 éléments: 1- instructions MIPS, 2- environnement
     (define ce (comp body env fp-sp))
     (list
      (append
       (first ce))
      (second ce)))

    ;; Bloc de fonction qui agit comme un bloc classique mais qui retourne également 
    ;; une valeur (A faire !)
    ((Funblock body ret)
     (foldl (lambda (expr acc)
              (let ((ce (comp expr (second acc) fp-sp)))
                (list (append (first acc)
                               (first ce))
                       (second ce))))
            (list '() env)
            body))

    ;; Bloc d'instructions utilisé pour le corps des conditions, 
    ;; des boucles, ou des fonctions.
    ((Block body)
     ;; On compile chaque expression en mettant dans une liste composée
     ;; de deux éléments : 1- l'expression MIPS, 2- l'environnement
     (foldl (lambda (expr acc)
              (let ((ce (comp expr (second acc) fp-sp)))
                (list (append (first acc)
                               (first ce))
                       (second ce))))
            (list '() env)
            body))


    ;; Déclaration d'une variable
    ((Let n v)
     ;; Compilation de la valeur v 
     (define ce (comp v (hash-set env n (Mem fp-sp 'fp)) fp-sp))

     (list
      (append
       ;; on récupère seulement l'instruction MIPS
       (first ce)
       ;; on empile la variable locale :
       (list (Addi 'sp 'sp -4)
             (Sw 'v0 (Mem 0 'sp))))
      (second ce)))

    ;; Référence à une variable
    ((Var n)
     ;; on met la valeur de la variable dans v0 :
     (list
      (list (Lw 'v0 (hash-ref env n)))
      env))))



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
  (printf "\n.text\n.globl main\nmain:\n")) ;; A RETIRER!

(mips-data (make-hash '((str_123 . "coucou") (nl . "\n"))))
