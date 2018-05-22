	.file	"calc.c"
	.text
.globl calc
	.type	calc, @function
calc:
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %edx	/*x into %edx*/
	movl	16(%ebp), %ecx	/*z into %ecx*/
	imull	$3, %edx		/*3x into %edx*/
	movl	12(%ebp), %eax	/*y into %eax*/
	imull	$7, %eax		/*7y into %eax*/
	addl	%edx, %eax		/*3x+7y into %eax*/
	movl	%ecx, %edx		/*z into %edx*/
	imull	$14, %edx		/*14z into %edx*/
	addl	%edx, %eax		/*3x+7y+14z into %eax*/
	popl	%ebp
	ret
	.size	calc, .-calc
	.ident	"GCC: (Ubuntu 4.3.3-5ubuntu4) 4.3.3"
	.section	.note.GNU-stack,"",@progbits
