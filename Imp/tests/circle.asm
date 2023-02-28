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
	sw $t3, 0($sp)
	subi $sp, $sp, 4
	sw $t2, 0($sp)
	subi $sp, $sp, 4
	sw $a0, 0($sp)
	subi $sp, $sp, 4
	sw $v0, 0($sp)
	subi $sp, $sp, 4
	li $t2, 0
	move $t2, $t2
	li $t2, 10
	move $t2, $t2
	li $t2, 10
	la $t0, retour
	sw $t2, 0($t0)
	li $t2, 32
	la $t0, espace
	sw $t2, 0($t0)
	b __lab_7
__lab_8:
	move $t2, $t2
	sw $t2, 0($sp)
	subi $sp, $sp, 4
	move $t2, $t2
	sw $t2, 0($sp)
	subi $sp, $sp, 4
	jal ligne
	addi $sp, $sp, 8
	la $t3, retour
	lw $t3, 0($t3)
	move $a0, $t3
	li $v0, 11
	syscall
	move $t2, $t2
	addi $t2, $t2, 1
	move $t2, $t2
__lab_7:
	move $t3, $t2
	move $t2, $t2
	addi $t2, $t2, 1
	slt $t2, $t3, $t2
	bnez $t2, __lab_8
	li $v0, 0
__lab_6:
	lw $v0, 4($sp)
	addi $sp, $sp, 4
	lw $a0, 4($sp)
	addi $sp, $sp, 4
	lw $t2, 4($sp)
	addi $sp, $sp, 4
	lw $t3, 4($sp)
	addi $sp, $sp, 4
	move $sp, $fp
	lw $ra, -4($fp)
	lw $fp, 0($fp)
	jr $ra
ligne:
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	subi $sp, $sp, 4
	addi $fp, $sp, 8
	sw $t5, 0($sp)
	subi $sp, $sp, 4
	sw $t3, 0($sp)
	subi $sp, $sp, 4
	sw $t4, 0($sp)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	subi $sp, $sp, 4
	sw $t2, 0($sp)
	subi $sp, $sp, 4
	sw $a0, 0($sp)
	subi $sp, $sp, 4
	sw $v0, 0($sp)
	subi $sp, $sp, 4
	li $t3, 0
	move $t2, $t3
	b __lab_2
__lab_3:
	lw $t4, 4($fp)
	lw $t3, 4($fp)
	mul $t3, $t4, $t3
	move $t5, $t2
	move $t4, $t2
	mul $t4, $t5, $t4
	add $t6, $t3, $t4
	lw $t4, 8($fp)
	lw $t3, 8($fp)
	mul $t3, $t4, $t3
	slt $t3, $t6, $t3
	beqz $t3, __lab_4
	li $t2, 46
	move $a0, $t2
	li $v0, 11
	syscall
	b __lab_5
__lab_4:
	li $t3, 35
	move $a0, $t3
	li $v0, 11
	syscall
__lab_5:
	la $t3, espace
	lw $t3, 0($t3)
	move $a0, $t3
	li $v0, 11
	syscall
	move $t3, $t2
	addi $t3, $t3, 1
	move $t2, $t3
__lab_2:
	move $t5, $t2
	lw $t3, 8($fp)
	addi $t3, $t3, 1
	slt $t3, $t5, $t3
	bnez $t3, __lab_3
	li $v0, 0
__lab_1:
	lw $v0, 4($sp)
	addi $sp, $sp, 4
	lw $a0, 4($sp)
	addi $sp, $sp, 4
	lw $t2, 4($sp)
	addi $sp, $sp, 4
	lw $t6, 4($sp)
	addi $sp, $sp, 4
	lw $t4, 4($sp)
	addi $sp, $sp, 4
	lw $t3, 4($sp)
	addi $sp, $sp, 4
	lw $t5, 4($sp)
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
retour:
	.word 0
espace:
	.word 0
