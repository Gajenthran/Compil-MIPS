# Compilateur-C

## Introduction

Le projet consiste à réaliser un compilateur Python vers MIPS en Racket. Cependant, 
j’ai plutôt opté pour créer un compilateur C.
Afin de vérifier la validité du code, le programme va être analysé lexicalement, syntaxiquement et sémantiquement avant d’être compilé.
L’analyse lexicale découpe le texte du code source en une suite de lexèmes. Il permet de vérifier les différents "mots" de notre langage.
L’analyse syntaxique permet de comprendre et de vérifier la structure du code. Cette partie va définir la structure de notre code (définir sa grammaire).
L’analyse sémantique va nous permettre de vérifier le sens de notre code à savoir le bon nombre d’arguments, le type ou encore la portée des variables, pour à la fin produire un arbre de syntaxe abstraite (AST) qui va être utiliser lors de la compilation. 
Une fois l’AST créé, on va devoir compiler l’ensemble des expressions à l’aide de cette dernière.

## Exécution du code
```
racket liec.rkt <fichier.liec> > test.s 
spim -f test.s.
```