	j	_min_caml_start
fib.10:
	li	r3, 1
	bgt	r2, r3, ble_else.24
	jr	r31
ble_else.24:
	addi	r3, r2, -1
	sw	r2, 0(r29)
	move	r2, r3
	sw	r31, -4(r29)
	addi	r29, r29, -8
	jal	fib.10
	addi	r29, r29, 8
	lw	r31, -4(r29)
	lw	r3, 0(r29)
	addi	r3, r3, -2
	sw	r2, -4(r29)
	move	r2, r3
	sw	r31, -8(r29)
	addi	r29, r29, -12
	jal	fib.10
	addi	r29, r29, 12
	lw	r31, -8(r29)
	lw	r3, -4(r29)
	add	r2, r3, r2
	jr	r31
_min_caml_start: # main entry point
	addi	r29, r0, 1
	sll	r29, r29, 20
   # main program start
	li	r2, 30
	sw	r31, 0(r29)
	addi	r29, r29, -4
	jal	fib.10
	addi	r29, r29, 4
	lw	r31, 0(r29)
	sw	r31, 0(r29)
	addi	r29, r29, -4
	jal	min_caml_print_int
	addi	r29, r29, 4
	lw	r31, 0(r29)
   # main program end
	halt
min_caml_print_int:
	ow	r2
	jr	r31
