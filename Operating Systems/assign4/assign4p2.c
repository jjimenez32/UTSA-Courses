#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include "nqueens_library.h"

/*************************************************************************** 
   assign4part2.c by Jonthan Jimenez
Purpose:
   This program simulates the Eight Queens Puzzle and tests part 1
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
   int flag;
   unsigned long solutions = 0;
   j = atoi(argv[1]) + 1;
   if(argc < 2){
   fprintf(stderr, "Too many arguments,\n");
   fprintf(stderr, "Proper usage: %s [int value]\n", argv[0]);
   exit(1);
   }
     
   printf("Assignment 4 part 2\n\n");
   

   for(i = 1; i < j; i++){
      //flag = i < 6 ? TRUE : FALSE;
      flag = i;
      //flag = 0;
      solutions = generate_n_queens_serial(i, flag);
      printf("Total amount of Solutions for %d numbers: %ld\n\n", i, solutions);
   }
   
    return 0;
}
