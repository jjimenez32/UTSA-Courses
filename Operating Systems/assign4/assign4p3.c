#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include <time.h>
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
   int n;
   int flag;
   unsigned long solutions = 0;
   n = atoi(argv[1]) + 1;
   thread_info_t th;

   if(argc < 2){
   fprintf(stderr, "Too many arguments,\n");
   fprintf(stderr, "Proper usage: %s [int value]\n", argv[0]);
   exit(1);
   }
   
   printf("Assignment 4 part 3\n\n");
   for(i = 1; i < n; i++){
      clock_t begin = clock();
      solutions = 0;
      for(j =0; j < i; j++){
         th.n = i;
         th.first = j;
         th.out_flag = 0;
         queens_thread((void *) &th);
         solutions += th.result;
      }
      clock_t end = clock();
      double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

      printf("Total amount of Solutions for %d numbers: %ld\n\n", i, solutions);
      printf("Time: %lf\n\n", time_spent);
   }
   
    return 0;
}
