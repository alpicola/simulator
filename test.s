	j	main
max:
	blt	r4, r5, lt
	move	r2, r4
	jr	r31
lt:
	move	r2, r5
	jr	r31
main:
	li	r0, 0
	li	r4, 10
	li	r5, 20
	jal	max
	ow	r2
