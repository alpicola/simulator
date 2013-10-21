	j	_min_caml_start
dbl.38:
	fadd	f1, f1, f1
	jr	r31
iloop.54:
	lli	r3, 0
	bne	r2, r3, beq_else.117
	lli	r2, 1
	j	min_caml_print_int
beq_else.117:
	fsub	f3, f3, f4
	fadd	f3, f3, f5
	swf	f5, 0(r29)
	sw	r2, -4(r29)
	swf	f3, -8(r29)
	swf	f6, -12(r29)
	swf	f2, -16(r29)
	sw	r31, -20(r29)
	addi	r29, r29, -24
	jal	dbl.38
	addi	r29, r29, 24
	lw	r31, -20(r29)
	lwf	f2, -16(r29)
	fmul	f1, f1, f2
	lwf	f6, -12(r29)
	fadd	f2, f1, f6
	lwf	f1, -8(r29)
	fmul	f3, f1, f1
	fmul	f4, f2, f2
	fadd	f5, f3, f4
	fli	f7, 4.000000
	fbgt	f5, f7, fble_else.118
	lw	r2, -4(r29)
	addi	r2, r2, -1
	lwf	f5, 0(r29)
	j	iloop.54
fble_else.118:
	lli	r2, 0
	j	min_caml_print_int
xloop.44:
	lli	r4, 400
	bgt	r4, r2, ble_else.119
	jr	r31
ble_else.119:
	sw	r2, 0(r29)
	sw	r3, -4(r29)
	sw	r31, -8(r29)
	addi	r29, r29, -12
	jal	min_caml_float_of_int
	addi	r29, r29, 12
	lw	r31, -8(r29)
	sw	r31, -8(r29)
	addi	r29, r29, -12
	jal	dbl.38
	addi	r29, r29, 12
	lw	r31, -8(r29)
	fli	f2, 400.000000
	fdiv	f1, f1, f2
	fli	f2, 1.500000
	fsub	f1, f1, f2
	lw	r2, -4(r29)
	swf	f1, -8(r29)
	sw	r31, -12(r29)
	addi	r29, r29, -16
	jal	min_caml_float_of_int
	addi	r29, r29, 16
	lw	r31, -12(r29)
	sw	r31, -12(r29)
	addi	r29, r29, -16
	jal	dbl.38
	addi	r29, r29, 16
	lw	r31, -12(r29)
	fli	f2, 400.000000
	fdiv	f1, f1, f2
	fli	f2, 1.000000
	fsub	f6, f1, f2
	lli	r2, 1000
	fli	f1, 0.000000
	fli	f2, 0.000000
	fli	f3, 0.000000
	fli	f4, 0.000000
	lwf	f5, -8(r29)
	sw	r31, -12(r29)
	addi	r29, r29, -16
	jal	iloop.54
	addi	r29, r29, 16
	lw	r31, -12(r29)
	lw	r2, 0(r29)
	addi	r2, r2, 1
	lw	r3, -4(r29)
	j	xloop.44
yloop.40:
	lli	r3, 400
	bgt	r3, r2, ble_else.121
	jr	r31
ble_else.121:
	lli	r3, 0
	sw	r2, 0(r29)
	move	r27, r3
	move	r3, r2
	move	r2, r27
	sw	r31, -4(r29)
	addi	r29, r29, -8
	jal	xloop.44
	addi	r29, r29, 8
	lw	r31, -4(r29)
	lw	r2, 0(r29)
	addi	r2, r2, 1
	j	yloop.40
_min_caml_start: # main entry point
	addi	r29, r0, 1
	sll	r29, r29, 20
   # main program start
	lli	r2, 0
	sw	r31, 0(r29)
	addi	r29, r29, -4
	jal	yloop.40
	addi	r29, r29, 4
	lw	r31, 0(r29)
   # main program end
	halt
min_caml_float_of_int:
	li	r3, 8388608
	li	r4, 1258291200 # 0x4b000000 
	imvf	f4, r4         # 8388608.0
	bltz	r2, float_of_int.neg
	bge	r2, r3, float_of_int.large
	or	r2, r2, r4
	imvf	f2, r2
	fsub	f1, f2, f4
	jr	r31
float_of_int.large:
	srl	r5, r2, 23
	sll	r2, r2, 9
	srl	r2, r2, 9
	or	r2, r2, r4
	imvf	f2, r2
	fsub	f1, f2, f4
float_of_int.large.1:
	fadd	f1, f1, f4
	addi	r5, r5, -1
	bgtz	r5, float_of_int.large.1
	jr	r31
float_of_int.neg:
	neg	r2, r2
	bge	r2, r3, float_of_int.neg.large
	or	r2, r2, r4
	imvf	f2, r2
	fsub	f1, f2, f4
	fneg	f1, f1
	jr	r31
float_of_int.neg.large:
	srl	r5, r2, 23
	sll	r2, r2, 9
	srl	r2, r2, 9
	or	r2, r2, r4
	imvf	f2, r2
	fsub	f1, f2, f4
float_of_int.neg.large.1:
	fadd	f1, f1, f4
	addi	r5, r5, -1
	bgtz	r5, float_of_int.neg.large.1
	fneg	f1, f1
	jr	r31
min_caml_print_int:
	ow	r2
	jr	r31
