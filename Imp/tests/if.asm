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
	li $t4, 1
	beqz $t4, __lab_2
	li $t2, 0
	beqz $t2, __lab_4
	li $t2, 66
	move $s3, $t2
	b __lab_5
__lab_4:
	li $t2, 65
	move $s3, $t2
__lab_5:
	b __lab_3
__lab_2:
	li $t4, 67
	move $s3, $t4
__lab_3:
	move $a0, $s3
	li $v0, 11
	syscall
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
