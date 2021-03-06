	.file	"calc.c"
	.text
.globl calc
	.type	calc, @function
calc:
	pushl	%ebp			/*save parameters*/
	movl	%esp, %ebp		/*move stack pointer to %ebp*/
	movl	8(%ebp), %edx 	/*move x to %edx*/
	movl	16(%ebp), %ecx	/*move z to %ecx*/
	leal	(%edx,%edx,2), %edx /*%edx = 3x*/
	movl	12(%ebp), %eax	/*move y to %eax*/
	leal	(%edx,%eax,2), %eax/*%eax = 3x+2y  */
	movl	%ecx, %edx 		/*%edx = z*/
	sall	$4, %edx		/*%edx = 16z*/
	subl	%ecx, %edx		/*%edx = 16z - z = 15z*/
	addl	%edx, %eax		/*%eax = 3x+2y + 15z*/
	popl	%ebp			/*pop the previous pointer*/
	ret
	.size	calc, .-calc
	.ident	"GCC: (Ubuntu 4.3.3-5ubuntu4) 4.3.3"
	.section	.note.GNU-stack,"",@progbits
