.data

.text
.globl main
main:
move $fp, $sp
	_main:
la $v0, toto
addi $sp, $sp, -4
sw $v0, 0($sp)
li $v0, 43
move $a0, $v0
li $v0, 1
syscall
jr $ra
move $t5, $v0
li $v0, 1
move $a0, $t5
syscall
li $v0, 0
jr $ra
