	.file	"testcalc.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"x=%d, y=%d, z=%d, result=%d\n"
	.text
.globl main
	.type	main, @function
main:
	leal	4(%esp), %ecx	
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	subl	$36, %esp			/* allocate 9 words on the stack*/
	movl	$11, 8(%esp)		/*third parameter (11) on the stack*/
	movl	$6, 4(%esp)			/* second parameter (7) on the stack*/
	movl	$2, (%esp)			/*first parameter (5) on the stack*/
	call	calc				/*call the function calc*/
	movl	%eax, 20(%esp)		/*return value of calc is 6th parameter*/
	movl	$11, 16(%esp)		/*give 11 to the parameter z in calc()*/
	movl	$6, 12(%esp)		/*give 6 to the parameter y in calc()*/
	movl	$2, 8(%esp)			/*give 2 to the parameter x in calc()*/
	movl	$.LC0, 4(%esp)		/*format specification is second parameter*/
	movl	$1, (%esp)			/*1 is first parameter of __printf_chk*/
	call	__printf_chk
	addl	$36, %esp			/*restore stack*/
	popl	%ecx
	popl	%ebp
	leal	-4(%ecx), %esp
	ret
	.size	main, .-main
	.ident	"GCC: (Ubuntu 4.3.3-5ubuntu4) 4.3.3"
	.section	.note.GNU-stack,"",@progbits
