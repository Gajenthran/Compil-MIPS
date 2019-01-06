.data
nl: .asciiz "\n"
str_123: .asciiz "coucou"

.text
.globl main
main:
move $fp, $sp
	_main:
li $v0, 10
addi $sp, $sp, -4
sw $v0, 0($sp)
lw $v0, 0($fp)
addi $sp, $sp, -4
sw $v0, 0($sp)

move $t5, $v0
li $v0, 1
move $a0, $t5
syscall
li $v0, 0
jr $ra
