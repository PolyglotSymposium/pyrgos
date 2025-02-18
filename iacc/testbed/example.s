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
	push %rax
	mov 0(%rsp), %rax
	push %rax
	mov $1437, %rax
	pop %rbx
	cmp %rbx, %rax
	setz %al
	test %rax, %rax
	jz lbl0
	mov $111, %rax
	jmp lbl1
lbl0:
	mov $999, %rax
lbl1:
	lea 8(%rsp), %rsp
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
