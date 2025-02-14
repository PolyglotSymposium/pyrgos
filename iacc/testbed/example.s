__entry__:
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
	push %rax
	mov 16(%rsp), %rax
	pop %rbx
	add %rbx, %rax
	pop %rbx
	add %rbx, %rax
	lea 8(%rsp), %rsp
	lea 8(%rsp), %rsp
	lea 8(%rsp), %rsp
	ret
.LC0:
	.string	"%d\n"
	.globl	main
	.type	main, @function
main:
	call __entry__
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	xorl	%eax, %eax
	call	printf@PLT
	xorl	%eax, %eax
	ret
