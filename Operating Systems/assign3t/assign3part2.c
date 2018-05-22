/**********************************************************************
    assign3part2.c by Jonathan Jimenez
Purpose:
    This program simulates the management of page table and physical
    memory allocation and what happens in the OS kernel.
Command Parameters:
    This program requires 1 command line sequence file
Results:
    All output is written to file "output-part2". 
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
#include "assign3part2.h"
int main(int argc, char *argv[]){
   int fdin;
   int fdout;
   int i;
   unsigned long fsize;
   unsigned long mapsize;
   unsigned long *Access;
   unsigned long *Write;
   unsigned long pfFault = 0;
   unsigned long pageFault[1000000];
   unsigned long virtualPage;
   unsigned long offset;
   unsigned long physicalPage;
   struct stat st;
	
   InitializePageTable();
   InitializePhysical();
	
   stat(argv[1], &st);
   fsize = st.st_size;
	
   mapsize = ((fsize + (4096 - 1)) & ~(4096 - 1));
   if ((fdin = open(argv[1], O_RDONLY)) <= 0){
       fprintf(stderr,"error opening first file\n");
       exit(1);
   }   
   Access = (unsigned long *)mmap(0,mapsize, PROT_READ,MAP_PRIVATE,fdin,0);
   fdout = open("output-part2", O_RDWR | O_CREAT | O_TRUNC, 0644);
   lseek(fdout, fsize -1, SEEK_SET);
   write(fdout, "" ,1);
   Write = (unsigned long *)mmap(0,mapsize, PROT_READ |PROT_WRITE, MAP_SHARED, fdout,0);
	
   for(i = 0; i < fsize/sizeof(unsigned long); i++){
      virtualPage = Access[i] >> 7;
      offset = Access[i] & 0x007f;		
      physicalPage = getPhysicalPage(virtualPage);
         if (physicalPage == 1000000 -1){
            pfFault++;
            pageFault[virtualPage]++;
            physicalPage = findFree();
         }
      if (physicalPage == 1000000 -1) physicalPage = evictLRU();		
      setVirtualPage(physicalPage, virtualPage);		
      unsigned long unshift = physicalPage << 7;		
      unsigned long new = unshift + offset;		
      Write[i] = new;		
      IncrementTimer();
   }
   msync(Write,mapsize,MS_SYNC);
   munmap(Access,mapsize);
   munmap(Write, mapsize);
   close(fdin);
   close(fdout);
   fprintf(stderr,"Page Faults: %ld\n", pfFault);
}
