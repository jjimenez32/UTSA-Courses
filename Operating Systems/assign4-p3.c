#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include "nqueens_library.h"

/*************************************************************************** 
   assign4part3.c by Jonthan Jimenez
Purpose:
   This program simulates the Eight Queens Puzzle and tests part 3
   of the assignment
Command Parameters:
   The programs requires 1 command line int value
Resulsts:
   All output is written to stdout
Returns: N/A
Notes:

***************************************************************************/
int main(int argc, char *argv[]){
   int i; 
   int j; 
   int k;
   int flag;
   unsigned long solutions = 0;
   j = atoi(argv[1]) + 1;
   thread_info_t th;

   if(argc < 2){
   fprintf(stderr, "Too many arguments,\n");
   fprintf(stderr, "Proper usage: %s [int value]\n", argv[0]);
   exit(1);
   }
   
   printf("Assignment 4 part 3\n\n");

   for(i = 1; i < k; i++){
      solutions = 0;
      for(j =0; j < i; j++){
         th.n = i;
         th.first = j;
         th.out_flag = 0;
         queens_thread_nothread((void *) &th);
         solutions += th.result;        
      }
      fprintf(stdout, "Number of Solutions for %d numbers: %ld\n\n", i, solutions);
   }
   
    return 0;
}
