.data
nl: .asciiz "\n"
str_123: .asciiz "coucou"

.text
.globl main
main:
li $v0, 99
addi $sp, $sp, -4
sw $v0, 0($sp)
