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
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	subi $sp, $sp, 4
	addi $fp, $sp, 8
	sw $t4, 0($sp)
	subi $sp, $sp, 4
	sw $t3, 0($sp)
	subi $sp, $sp, 4
	sw $a0, 0($sp)
	subi $sp, $sp, 4
	sw $v0, 0($sp)
	subi $sp, $sp, 4
	sw $s3, 0($sp)
	subi $sp, $sp, 4
	sw $s4, 0($sp)
	subi $sp, $sp, 4
	li $t3, 1
	move $s3, $t3
	li $t3, 64
	move $s4, $t3
	move $t4, $s3
	move $t3, $s4
	add $t3, $t4, $t3
	move $a0, $t3
	li $v0, 11
	syscall
	li $v0, 0
__lab_1:
	lw $s4, 4($sp)
	addi $sp, $sp, 4
	lw $s3, 4($sp)
	addi $sp, $sp, 4
	lw $v0, 4($sp)
	addi $sp, $sp, 4
	lw $a0, 4($sp)
	addi $sp, $sp, 4
	lw $t3, 4($sp)
	addi $sp, $sp, 4
	lw $t4, 4($sp)
	addi $sp, $sp, 4
	move $sp, $fp
	lw $ra, -4($fp)
	lw $fp, 0($fp)
	jr $ra
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
