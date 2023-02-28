.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
init_end:
	move $a0, $v0
	jal main
	li $v0, 10
	syscall
main:
	sw $t2, 0($sp)
	subi $sp, $sp, 4
	li $t5, 0
	move $s3, $t5
	li $t5, 65
	move $s4, $t5
	li $t5, 5
	slt $t5, $s3, $t5
	b __lab_2
__lab_3:
	addi $t5, $s3, 1
	move $s3, $t5
	move $a0, $s4
	li $v0, 11
	syscall
	addi $t5, $s4, 1
	move $s4, $t5
__lab_2:
	li $t5, 5
	slt $t5, $s3, $t5
	bnez $t5, __lab_3
#built-in atoi
atoi:
	li $v0, 0
atoi_loop:
	beqz $t0, atoi_end
	addi $t0, $t0, -48
	bltz $t0, atoi_error
	bge $t0, 10, atoi_error
	mul $v0, $v0, 10
	add $v0, $v0, $t0
	addi $a0, $a0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	jr $ra
.data
