.data
nl: .asciiz "\n"
str_123: .asciiz "coucou"

.text
.globl main
main:
move $fp, $sp
li $v0, 92
addi $sp, $sp, -4
sw $v0, 0($sp)
li $v0, 42
lw $t0, 0($sp)
addi $sp, $sp, 4
move $t1, $v0
seq $t9, $t0, $t1
addi $sp, $sp, -4
sw $v0, 0($sp)
	loop_1:
beqz $t9, endloop_1
li $v0, 0
b loop_1
move $t5, $v0
li $v0, 1
move $a0, $t5
syscall
li $v0, 0
jr $ra
