	.file	"calc.c"
	.text
.globl calc
	.type	calc, @function
calc:
	pushl	%ebp
	movl	%esp, %ebp
	movl 	8(%ebp), %edx		/*x into %edx*/
	movl	16(%ebp), %ecx		/*z into %ecx*/
	movl	%edx, %eax			/*%eax has x */
	sall	$2, %edx			/*4x into %edx*/
	subl	%eax, %edx			/*4x-x = 3x into %edx*/
	movl  %ecx, %eax		     /*z into %eax*/
	sall	$4, %ecx			/*16z into %ecx*/
	subl	%eax, %ecx			/*15z into %ecx*/
	subl 	%eax, %ecx			/*14z into %ecx*/
	addl	%edx ,%ecx     		/*3x+14z into %ecx*/
	movl	12(%ebp), %eax		/*y into %eax*/
	movl    %eax, %edx		/*y into %edx*/
	sall	$3, %eax			/*8y into %eax*/
	subl	%edx, %eax			/*7y into %eax*/
	addl	%ecx, %eax			/*3x+14z+7y into %eax*/
	popl	%ebp
	ret
	.size	calc, .-calc
	.ident	"GCC: (Ubuntu 4.3.3-5ubuntu4) 4.3.3"
	.section	.note.GNU-stack,"",@progbits
