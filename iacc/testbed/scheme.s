	.file	"scheme.c"
	.text
	.p2align 4
	.globl	scheme_entry
	.type	scheme_entry, @function
scheme_entry:
.LFB0:
	.cfi_startproc
	mov $42, %rax
	push %rax
	mov $16, %rax
	mov %rax, %rbx
	pop %rax
	sub %rbx, %rax
	ret
	.cfi_endproc
.LFE0:
	.size	scheme_entry, .-scheme_entry
	.ident	"GCC: (GNU) 14.2.1 20240910"
	.section	.note.GNU-stack,"",@progbits
