	.file	"scheme.c"
	.text
	.p2align 4
	.globl	scheme_entry
	.type	scheme_entry, @function
scheme_entry:
.LFB0:
	.cfi_startproc
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
	.cfi_endproc
.LFE0:
	.size	scheme_entry, .-scheme_entry
	.ident	"GCC: (GNU) 14.2.1 20240910"
	.section	.note.GNU-stack,"",@progbits
