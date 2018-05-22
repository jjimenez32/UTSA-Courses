/**********************************************************************
    assign3part1.c by Jonathan Jimenez
Purpose:
    This program simulates the management of page table and physical
    memory allocation and what happens in the OS kernel.
Command Parameters:
    This program requires 1 command line sequence file
Results:
    All output is written to file "output-part1". 
Returns: N/A
Notes:
   
**********************************************************************/
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
int main(int argc, char *argv[]){
   int pages[7] = {2,4,1,7,3,5,6};
   int fdin;
   int fdout;
   int count = 0;
   int num;
   unsigned long address;
   unsigned long offset;
   unsigned long physical;
   
   if ((fdin = open(argv[1], O_RDONLY)) <= 0){
       fprintf(stderr,"error opening first file\n");
       exit(1);
   }
   fdout = open("output-part1", O_WRONLY | O_CREAT, 0600);
   printf("got here after opens\n"); 
   while ((num = read(fdin, &address, sizeof(address))) > 0){
      physical = address >> 7;
      physical = pages[physical];
      offset = address & 127;
      physical = physical << 7;
      physical = physical | offset;
      printf("got here\n");
      if (( num = write( fdout, &physical, sizeof(physical))) < 0){
         perror("error writing\n");
         close(fdout);
         exit(1);
      }
      count ++;
   }
   close (fdin);

   exit(0);
}
