	.file	"input.c"
	.intel_syntax noprefix
	.text
	.p2align 4
	.globl	example
	.type	example, @function
example:
.LFB0:
	.cfi_startproc
	endbr64
	lea	rcx, 80[rdi]
	xor	eax, eax
	.p2align 4,,10
	.p2align 3
.L2:
	mov	rdx, QWORD PTR [rdi]
	add	rdi, 8
	lea	rax, [rax+rdx*2]
	cmp	rdi, rcx
	jne	.L2
	ret
	.cfi_endproc
.LFE0:
	.size	example, .-example
	.ident	"GCC: (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	 1f - 0f
	.long	 4f - 1f
	.long	 5
0:
	.string	 "GNU"
1:
	.align 8
	.long	 0xc0000002
	.long	 3f - 2f
2:
	.long	 0x3
3:
	.align 8
4:
