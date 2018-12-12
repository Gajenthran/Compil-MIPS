#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide liec-lexer
         keywords
         operators
         punctuations
         atoms
         (struct-out position))

;; token declarations

(define-empty-tokens keywords
  (Llet Lrec
   Lif Lthen Lelse
   Lbegin Lend
   Lnil
   Leof))

(define-empty-tokens operators
  (Leq Lneq Llt Lgt Llte Lgte Lassign
   Ladd Lsub Lmul Ldiv Lmod
   Land Lor Lnot Lxor))

(define-empty-tokens punctuations
  (Lsc Lcc Lopar Lcpar Lobra Lcbra
   Lcol Larrow Lcom Llist))

(define-tokens atoms
  (Lident Lnum Lstr Lbool Ltype))

;; regexp abbreviations

(define-lex-abbrev latin_
  (:or (char-range "a" "z")
       (char-range "A" "Z")
       "_"))

(define-lex-abbrev latin_num
  (:or latin_ numeric))

(define-lex-abbrev ident
  (:: latin_ (:* latin_num)))

(define-lex-abbrev number
  (:: (:+ numeric) (:? "." (:+ numeric))))

(define-lex-abbrev bool
  (:or "true" "false"))

(define-lex-abbrev types
  (:or "num" "str" "bool" "nil"))


;; lexer

(define liec-lexer
  (lexer-src-pos
   ((eof)      (token-Leof))
   (whitespace (return-without-pos (liec-lexer input-port)))
   ("//"       (return-without-pos (comment-lexer input-port)))
   ("let"      (token-Llet))
   ("rec"      (token-Lrec))
   (":"        (token-Lcol))
   (","        (token-Lcom))
   ("<-"       (token-Larrow))
   ("if"       (token-Lif))
   ("then"     (token-Lthen))
   ("else"     (token-Lelse))
   ("begin"    (token-Lbegin))
   ("end"      (token-Lend))
   ("=="       (token-Leq))
   ("!="       (token-Lneq))
   ("<"        (token-Llt))
   (">"        (token-Lgt))
   ("<="       (token-Llte))
   (">="       (token-Lgte))
   ("="        (token-Lassign))
   ("+"        (token-Ladd))
   ("-"        (token-Lsub))
   ("*"        (token-Lmul))
   ("/"        (token-Ldiv))
   ("%"        (token-Lmod))
   ("and"      (token-Land))
   ("or"       (token-Lor))
   ("not"      (token-Lnot))
   (";"        (token-Lsc))
   ("::"       (token-Lcc))
   ("("        (token-Lopar))
   (")"        (token-Lcpar))
   ("["        (token-Lobra))
   ("]"        (token-Lcbra))
   ("()"       (token-Lnil))
   (types      (token-Ltype (string->symbol lexeme)))
   ("list"     (token-Llist))
   (bool       (token-Lbool (string=? "true" lexeme)))
   (number     (token-Lnum (string->number lexeme)))
   ("\""       (token-Lstr (apply string-append (string-lexer input-port))))
   (ident      (token-Lident (string->symbol lexeme)))
   (any-char   (begin
                 (eprintf "Lexer: ~a: unrecognized char at line ~a col ~a.\n"
                          lexeme (position-line start-pos) (position-col start-pos))
                 (exit 1)))))

(define string-lexer
  (lexer
   ("\\\""   (cons "\"" (string-lexer input-port)))
   ("\\\\"   (cons "\\" (string-lexer input-port)))
   ("\\n"    (cons "\n" (string-lexer input-port)))
   ("\\t"    (cons "\t" (string-lexer input-port)))
   ("\""     '())
   (any-char (cons lexeme (string-lexer input-port)))))

(define comment-lexer
  (lexer
   ("\n"     (liec-lexer input-port))
   (any-char (comment-lexer input-port))))
