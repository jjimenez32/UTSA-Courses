/**********************************************************************
    assign1-0.c by Jonathan Jimenez
Purpose:
    This program precedes and is preperation for the process scheduling 
	program
Command Parameters:
    This program requires 7 command line integers to be passed in

Results:
    All output is written to stdout. 
Returns: N/A
Notes:
   
**********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "pslibrary.h"

int main (int argc, char *argv[]){
   int i = 0;
   int total = 0;
   int newv[7];

   if (argc != 8 ){
      printf("Not correct amount of arguments\n");
      printf("Correct way: assign1-0 int int int int int int int");
      exit(1);
   }

   printf("Assignment 1 program was written by Jonathan Jimenez\n");
   *argv++;
   printf("inputs: ");
   while ( *argv ) {
      if (i >= 1) {
         newv[i] = atoi(*argv);
	 total += newv[i];
      }   		 
      printf("%s ",*argv);	
      *argv++;
      i++;
   }
   printf("\n");

   char s1[total];
   char s2[total];
   part0(s1,s2);		
   display("Part 0\n",s1,s2);
   
   exit(0);
}
