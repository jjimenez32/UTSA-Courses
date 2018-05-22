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
/******************** InitializePhysical ************************************
    void InitializePhysical(){
Purpose:
    Starts the simulation and initializes the physical page 
Parameters:
    n/a
Returns:
    n/a
Notes:
**************************************************************************/
void InitializePhysical(){
   unsigned long i;
   freePage[0] = 1;
   
   for(i = 1; i < 7; i++)
      freePage[i]=0;
}
/******************** findFree *******************************************
    unsigned long findFree(){
Purpose:
    Finds a free page
Parameters:
    n/a
Returns:
    i   the found free page
Notes:
**************************************************************************/
unsigned long findFree(){
   unsigned long i;
   for(i = 1; i < 7; i++){
      if (freePage[i] == 0){
         freePage[i] == 1;
         return i;
	  }
   }
   return 1000000 - 1;
}
/******************** evictLRU *******************************************
    unsigned long evictLRU(){
Purpose:
    Function for evicting the LRU and returns the physical page 
Parameters:
    n/a
Returns:
    physicalPage
Notes:
**************************************************************************/
unsigned long evictLRU(){
   unsigned long i;
   unsigned long j;
   unsigned long LRU = 0;
   unsigned long physicalPage = 1000000 - 1;
   for(i = 1; i < 7; i++){
      j = getTimer(i);
	  if (j > LRU){
         physicalPage = i;
         LRU = j;
	  }
   }
   return physicalPage;
}
