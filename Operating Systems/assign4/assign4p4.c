#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include <time.h>
#include "nqueens_library.h"
/*************************************************************************** 
   assign4part4.c by Jonthan Jimenez
Purpose:
   This program simulates the Eight Queens Puzzle and tests part 4
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
   int tc;
   unsigned long *solutions;
   thread_info_t *th;
   pthread_t *pth;
   pthread_attr_t attr;
   void *stat;
   n = atoi(argv[1]) + 1;
   solutions = (unsigned long *)malloc(n * sizeof(unsigned long));
   th = (thread_info_t *)malloc(n*sizeof(thread_info_t));
   pth = (pthread_t *)malloc(n*sizeof(pthread_t));

   if(argc < 2){
   fprintf(stderr, "Too many arguments,\n");
   fprintf(stderr, "Proper usage: %s [int value]\n", argv[0]);
   exit(1);
   }
     
   printf("Assign 4 part 4 \n\n");
   
   pthread_attr_init(&attr);
   pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

   for(i = 1; i < n; i++){
      clock_t begin = clock();
      solutions[i] = 0;
      
      for(j =0; j < i; j++){
         th[j].n = i;
         th[j].first = j;
         th[j].out_flag = 0;
         pthread_create(&pth[j], &attr, queens_thread2, (void *) &th[j]);
      }
      
      for(j=0; j < i; j++){
         pthread_join(pth[j], &stat);
         thread_info_t * th = (thread_info_t *)(stat);
         solutions[i] += th->result;        
      }
      clock_t end = clock();
      double time = (double)(end - begin) / CLOCKS_PER_SEC;
      fprintf(stdout,"Total amount of solutions for %d numbers: %ld\n", i, solutions[i]);
      printf("Time: %lf\n\n", time);
   }
   pthread_attr_destroy(&attr);
   pthread_exit(NULL);
   free(solutions);
   free(th);
   free(pth);
   
   return 0;
}
    
   
