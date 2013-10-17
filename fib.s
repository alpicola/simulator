	j	main
fib:
	addi	r4, r4, -1
	blez	r4, le
	addi	r29, r29, -24
	sw	r18, 16(r29)
	sw	r31, 20(r29)
	move	r18, r4
	jal	fib
	move	r8, r2
	addi	r4, r18, -1
	move	r18, r8
	jal	fib
	add	r2, r2, r18
	lw	r18, 16(r29)
	lw	r31, 20(r29)
	addi	r29, r29, 24
	jr	r31
le:
	lli	r2, 1
	jr	r31
main:
	addi	r29, r0, 1
	sll	r29, r29, 20
	move	r30, r29
	lli	r4, 30
	jal	fib
	ow	r2
