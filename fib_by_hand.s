	j	main
fib:
	addi	r8, r4, -1
	blez	r8, le
	addi	r29, r29, -24
	sw	r18, 16(r29)
	sw	r31, 20(r29)
	move	r4, r8
	move	r18, r8
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
	move	r2, r4
	jr	r31
main:
	addi	r29, r0, 1
	sll	r29, r29, 20
	lli	r4, 30
	jal	fib
	ow	r2
