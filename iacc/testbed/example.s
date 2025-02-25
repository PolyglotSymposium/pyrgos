__entry__:
	mov $10485760, %rdi
	mov $12, %rax
	syscall
	cmp $-1, %rax
	je heap_allocation_error
	mov %rax, %rsi
	mov $36, %rax
	push %rax
	mov $22, %rax
	pop %rbx
	add %rbx, %rax
	push %rax
	mov 0(%rsp), %rax
	push %rax
	mov $16, %rax
	mov %rax, %rbx
	pop %rax
	sub %rbx, %rax
	push %rax
	mov $1337, %rax
	push %rax
	mov 8(%rsp), %rax
	push %rax
	mov 24(%rsp), %rax
	pop %rbx
	add %rbx, %rax
	push %rax
	mov 8(%rsp), %rax
	pop %rbx
	add %rbx, %rax
	push %rax
	mov 0(%rsp), %rax
	push %rax
	mov $1437, %rax
	pop %rbx
	cmp %rbx, %rax
	je lbl0
	mov 0(%rsp), %rax
	push %rax
	mov $1337, %rax
	pop %rbx
	cmp %rbx, %rax
	je lbl2
	mov $999, %rax
	jmp lbl3
lbl2:
	mov $222, %rax
lbl3:
	jmp lbl1
lbl0:
	mov $111, %rax
lbl1:
	lea 8(%rsp), %rsp
	lea 8(%rsp), %rsp
	lea 8(%rsp), %rsp
	lea 8(%rsp), %rsp
	ret
heap_allocation_error:
	mov $1, %rdi
	mov $60, %rax
	syscall
.LC0:
	.string	"%d\n"
	.globl	main
	.type	main, @function
main:
	call __entry__
	mov	%rax, %rsi
	lea	.LC0(%rip), %rdi
	xor	%rax, %rax
	call	printf@PLT
	xor	%rax, %rax
	ret
