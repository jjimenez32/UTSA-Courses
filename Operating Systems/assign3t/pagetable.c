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
/******************** setVirtualPage ************************************
    char setVirtualPage(unsigned long physicalPage, unsigned long 
	virtualPage
Purpose:
    Sets a page in the page table. 
Parameters:
    O  unsigned long	physicalPage
	O  unsinged long 	virtualPage
Returns:
    1 on success
	0 on failure
Notes:
**************************************************************************/
char setVirtualPage(unsigned long physicalPage, unsigned long virtualPage){
   if (physicalPage < 7){
      pageTimer[physicalPage] = 0;
	  pageTable[physicalPage] = virtualPage;
      return 1;
   }
   else
      return 0;
}
/******************** getVirtualPage **********************************************
    unsigned long getVirtualPage(unsigned long physicalPage){
Purpose:
    Simply returns the virtual page number associated with physical page number
Parameters:
    O  unsigned long      physicalPage
Returns:
    n/a
Notes:
**************************************************************************/
unsigned long getVirtualPage(unsigned long physicalPage){
   return pageTable[physicalPage];
}
/******************** getPhysicalPage **********************************************
	unsigned long getPhysicalPage(unsigned long virtualPage){
Purpose:
    Find physical page depending on the virtual page number
Parameters:
    O  unsigned long      virtualPage
Returns:
    i   physical page number
Notes:
**************************************************************************/
unsigned long getPhysicalPage(unsigned long virtualPage){
   unsigned long i;
   for(i = 1; i < 7; i++){
      if(pageTable[i] == virtualPage)
		  return i;
   }
   
   return 1000000 - 1;
}
/******************** getTimer **********************************************
	unsigned long getTimer(unsigned long physicalPage){
Purpose:
    returns the page timer depending physical page
Parameters:
    O  unsigned long      physicalPage
Returns:
    pageTimer
Notes:
**************************************************************************/
unsigned long getTimer(unsigned long physicalPage){
    return pageTimer[physicalPage];
}
/******************** InitializePageTable **********************************************
    void InitializePageTable(){
Purpose:
    Starts the simulation and initializes the page table
Parameters:
    n/a
Returns:
    n/a
Notes:
**************************************************************************/
void InitializePageTable(){
   pageTimer[0] = 0;
   pageTable[0] = 10000000 - 2;
   unsigned long i;
   for (i = 0; i < 7; i++){
      pageTimer[i] = 0;
	  pageTable[i] = 1000000;
   }
}
/******************** IncrementTimer **********************************************
    void IncrementTimer(){
Purpose:
    Increments the page timers
Parameters:
    n/a
Returns:
    n/a
Notes:
**************************************************************************/
void IncrementTimer(){
   unsigned long i;
   for(i = 1; i < 7; i++)
	   pageTimer[i]++;
}
