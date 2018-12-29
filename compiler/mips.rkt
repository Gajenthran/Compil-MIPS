#lang racket/base

(provide (all-defined-out))

;;;;; MIPS
(struct Move (rd rs))
(struct Li (r i))
(struct La (r a))
(struct Addi (rd rs i))
(struct Add (rd rs1 rs2))
(struct Sub (rd rs1 rs2))
(struct Mult (rs1 rs2))
(struct Div (rs1 rs2))
(struct Lo (rd))
(struct Seq (rd rs1 rs2))
(struct Sgt (rd rs1 rs2))
(struct Slt (rd rs1 rs2))
(struct B (l))
(struct Beqz (rs l))
(struct Bnez (rs l))
(struct Sw (r loc))
(struct Lw (r loc))
(struct Syscall ())
(struct Jr (r))
(struct Label (l))

;; addresses
(struct Lbl (l))   ;; label (souvent présent dans .data)
(struct Mem (b r)) ;; emplacement mémoire à l'adresse b + valeur du registre r
