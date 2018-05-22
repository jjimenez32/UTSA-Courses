#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "pslibrary.h"

#define READY  0
#define RUNNING  1
#define WAITING  2
#define DONE  3

static char stateChars[] = {'r','R','w',0};


/******************** part0 **********************************************
    void part0(char *s1, char *s2){
Purpose:
    Copies a set string to the character string passed in 
Parameters:
    O  char*   s1 and s2     character strings passed in that will hold
	                         rwR string 
Returns:
    n/a
Notes:
    - Uses strcpy to copy the string to the pointer
**************************************************************************/
void part0(char *s1, char *s2){
   char *sc1 = "RRwwwwwRRRRRRRRR";
   char *sc2 = "rrRRRRwwwwwwwwrrRRRRRRR";
  
   strcpy(s1, sc1);
   strcpy(s2, sc2);

   return;
}
/******************** display *********************************************
    void smInit(StorageManager *pMgr)
Purpose:
    Displays the string depending on given values
Parameters:
    O char *   heading      the heading of the function call. Set to "part0"
	O char *   s1, s2       the strings passed in to print
Returns:
    n/a
Notes:
    - Uses malloc to actually allocate memory for the heap.
**************************************************************************/
void display(char *heading, char *s1, char *s2){
   float final;
   int len = 0;
   int rs1 = 0;
   int rs2 = 0;
   float Rs1 = 0;
   float Rs2 = 0;
   int s1len = 0; 
   int s2len = 0;
   
   //printing the default heading and string values passed in
   printf("\n");
   printf("%s",heading);
   printf("%s\n",s1);
   printf("%s\n",s2); 

   while (*s1 != '\0'){
      if ( *s1 == 114) {
         rs1++;
      }
      if ( *s1 == 82) {
         Rs1++;
      }
      *s1++;
      s1len++;
   }

   while ( *s2 != '\0') {
      if ( *s2 == 114) {
         rs2++;
      }
      if ( *s2 == 82) {
         Rs2++;
      }
      *s2++;
      s2len++;
   }

   if (s1len > s2len){
      len = s1len;
   }
   else if ( s2len > s1len) {
      len = s2len;
   } else {
      len = s1len;
   }
   printf("%i %i %.1lf %.5f \n", rs1, rs2, (rs1 + rs2)/2.0, (Rs1 + Rs2)/len);
}


/******************** fcfsa ***************************************************
    void fcfsa(char *s1,char *s2,int x1,int y1,int z1,int x2,int y2,int z2){
Purpose:
    
Parameters:
    O  char* s1, s2                   
    O  int   x1,y1,z1,x2,y2,z2                
Returns:
    n/a
Notes:
    1.The two strings strings will not have R's in the same position.
    2.No string will have an r in the position that the other string has an r 
	  or w.
    3.The first string will have the smallest number of r's that will allow it 
	  to satisfy conditions a) and b).
    4.The second string will have the smallest number of r's that will not 
	  contradict a), b), or c).
*******************************************************************************/
void fcfsa(char *s1,char *s2,int x1,int y1,int z1,int x2,int y2,int z2){
  while (x1 > 0 ) {
        *s1 = 82;
        *s1++;
        *s2 = 114;
        *s2++;
        x1--;
   }
   while (x2 > 0 ) {
      if ( (y1 == 0) && (z1 > 0) ) {
         *s1 = 114;
         *s1++;
      }
      else if ( y1 > 0 ) {
         *s1 = 119;
         *s1++;
         y1--;         
      }
      *s2 = 82;
      *s2++;
      x2--;
   }

   while( y1 > 0 ){
      if ( ( y2 == 0 ) && ( z2 > 0 )   ) {
         *s2 = 82;
         *s2++;
         z2--;
      } else if ( y2 > 0   ) {
         *s2 = 119;
         *s2++;
         y2--;
      }
      *s1 = 119;
      *s1++;
      y1--;
   }
   
   while( y2 > 0 ){
      if ( ( y1 == 0  ) && ( z1 > 0  )  ) {
         *s1 = 82;
         *s1++;
         z1--;
      } else if  ( y1 > 0  ) {
         *s1 = 119;
         *s1++;
         y1--;
         
      }
      *s2 = 119;
      *s2++;
      y2--;
      
   }
 
   while (z1 > 0) {
      if ( ( y2 == 0)  &&  (z2 > 0 )   ) {
         *s2 = 114;
         *s2++;
      } else if ( y2 > 0 ) {
         *s2 = 119;
         *s2++;
         y2--; 
      }
      *s1 = 82;
      *s1++;
      z1--;
   }

   while (z2 > 0 ) {
      if (  (y1 == 0) && ( z1 > 0  )  ) {
         *s1 = 114;
         *s1++;      
      } else if ( y1 > 0 ) {
         *s1 = 119;
         *s1++;
         y1--;
      }
      *s2 = 82;
      *s2++;
      z2--;
   }
   *s1 = '\0';
   *s2 = '\0';
}


/******************** proto ***************************************************
    void fcfs(char *s1,char *s2,int x1,int y1,int z1,int x2,int y2,int z2){
Purpose:
    
Parameters:
    O  char* s1, s2                   
    O  int   x1,y1,z1,x2,y2,z2                
Returns:
    n/a
Notes:
    Used Korkmaz's proto function
*******************************************************************************/
void fcfs (char *s1, char *s2, int x1, int y1, int z1, int x2, 
                                                           int y2, int z2) {
   int i;                                   /* next string position (time) */
   int state1 = READY;                            /* start with both ready */
   int state2 = READY;
   int cpuLeft1 = x1;                       /* P1 next CPU burst remaining */
   int cpuLeft2 = x2;                       /* P2 next CPU burst remaining */
   int ioLeft1 = y1;        /* P1 next IO burst remaining, 0 if no more IO */
   int ioLeft2 = y2;        /* P2 next IO burst remaining, 0 if no more IO */
   int qleft;                                         /* quantum remaining */

   for (i=0; (state1 != DONE) || (state2 != DONE); i++) {
                                /* running process completes its CPU burst */
      if ((state1 == RUNNING) && (cpuLeft1== 0)) {
                                                         
         if (ioLeft1 == 0) {
            state1 = DONE;
            s1[i] = stateChars[state1];            /* terminate the string */
         }
         else
            state1 = WAITING;
      }  
      else if ((state2 == RUNNING) && (cpuLeft2 == 0)) {
         if (ioLeft2 == 0){
            state2 = DONE;
            s2[i] = stateChars[state2];
         }
	 else
            state2 = WAITING;
      }  
                                     /* running process has quantum expire */
      if ((state1 == RUNNING) && (qleft == 0) ) {
         state1 = READY;
      }  
      if ((state2 == RUNNING) && (qleft == 0) ) {
         state2 = READY;
      }  
                                                     /* handle IO complete */
      if ((state1 == WAITING) && (ioLeft1 == 0)) {
         state1 = READY;
         cpuLeft1 = z1;
         
      }  
      if ((state2 == WAITING) && (ioLeft2 == 0)) {
         state2 = READY;
         cpuLeft2 = z2;
      }  
                                    /* if both ready, depends on algorithm */
      if ( (state1 == READY) && (state2 == READY) ) {
         state1 = RUNNING;
      }  
                                     /* handle one ready and CPU available */
      else if ( (state1 == READY) && (state2 != RUNNING)) {
         state1 = RUNNING;
      }  
      else if ( (state2 == READY) && (state1 != RUNNING)) {
         state2 = RUNNING;
      }  
         /* insert chars in string, but avoid putting in extra string terminators */
      if (state1 != DONE)
         s1[i] = stateChars[state1];
      if (state2 != DONE)
         s2[i] = stateChars[state2];
                                                        /* decrement counts */
      qleft--;                   /* OK to decrement even if nothing running */
      if (state1 == RUNNING)
         cpuLeft1--;
      if (state1 == WAITING)
         ioLeft1--;
      if (state2 == RUNNING)
         cpuLeft2--;
      if (state2 == WAITING)
         ioLeft2--;
   }                                               /* end of main for loop */

}
/******************** sjf ***************************************************
    void sjf(char *s1,char *s2,int x1,int y1,int z1,int x2,int y2,int z2){
Purpose:
    
Parameters:
    O  char* s1, s2                   
    O  int   x1,y1,z1,x2,y2,z2                
Returns:
    n/a
Notes:
    Used Korkmaz's proto method to build this function
*******************************************************************************/
void sjf(char *s1, char *s2, int x1, int y1, int z1, int x2, int y2, int z2){
                                              /* this prototype is genius! */
   int i;                                   /* next string position (time) */
   int state1 = READY;                            /* start with both ready */
   int state2 = READY;
   int cpuLeft1 = x1;                       /* P1 next CPU burst remaining */
   int cpuLeft2 = x2;                       /* P2 next CPU burst remaining */
   int ioLeft1 = y1;        /* P1 next IO burst remaining, 0 if no more IO */
   int ioLeft2 = y2;        /* P2 next IO burst remaining, 0 if no more IO */
   int qleft;                                         /* quantum remaining */

   for (i=0; (state1 != DONE) || (state2 != DONE); i++) {
                                /* running process completes its CPU burst */
      if ((state1 == RUNNING) && (cpuLeft1== 0)) {
                                                         
         if (ioLeft1 == 0) {
            state1 = DONE;
            s1[i] = stateChars[state1];            /* terminate the string */
         }
         else
            state1 = WAITING;
      }  
      else if ((state2 == RUNNING) && (cpuLeft2 == 0)) {
         if (ioLeft2 == 0){
            state2 = DONE;
            s2[i] = stateChars[state2];
         }
	 else
            state2 = WAITING;
      }  
                                     /* running process has quantum expire */
      if ((state1 == RUNNING) && (qleft == 0) ) {
         state1 = READY;
      }  
      if ((state2 == RUNNING) && (qleft == 0) ) {
         state2 = READY;
      }  
                                                     /* handle IO complete */
      if ((state1 == WAITING) && (ioLeft1 == 0)) {
         state1 = READY;
         cpuLeft1 = z1;
         
      }  
      if ((state2 == WAITING) && (ioLeft2 == 0)) {
         state2 = READY;
         cpuLeft2 = z2;
      }  
                                    /* if both ready, depends on algorithm */
      if ( (state1 == READY) && (state2 == READY) ) {
         if ( cpuLeft1 < cpuLeft2 ) {
            state1 = RUNNING;
         } else if ( cpuLeft1 > cpuLeft2 ) {
            state2 = RUNNING;
         } else {
            state1 = RUNNING;
         }
      }  
                                     /* handle one ready and CPU available */
      else if ( (state1 == READY) && (state2 != RUNNING)) {
         state1 = RUNNING;
      }  
      else if ( (state2 == READY) && (state1 != RUNNING)) {
         state2 = RUNNING;
      }  
         /* insert chars in string, but avoid putting in extra string terminators */
      if (state1 != DONE)
         s1[i] = stateChars[state1];
      if (state2 != DONE)
         s2[i] = stateChars[state2];
                                                        /* decrement counts */
      qleft--;                   /* OK to decrement even if nothing running */
      if (state1 == RUNNING)
         cpuLeft1--;
      if (state1 == WAITING)
         ioLeft1--;
      if (state2 == RUNNING)
         cpuLeft2--;
      if (state2 == WAITING)
         ioLeft2--;
   }                                               /* end of main for loop */     

}
/******************** psjf ***************************************************
    void psjf(char *s1,char *s2,int x1,int y1,int z1,int x2,int y2,int z2){
Purpose:
    
Parameters:
    O  char* s1, s2                   
    O  int   x1,y1,z1,x2,y2,z2                
Returns:
    n/a
Notes:
    Used Korkmaz's proto method to build this function
*******************************************************************************/
void psjf(char *s1, char *s2, int x1, int y1, int z1, int x2, int y2, int z2){
   int i;                                   /* next string position (time) */
   int state1 = READY;                            /* start with both ready */
   int state2 = READY;
   int cpuLeft1 = x1;                       /* P1 next CPU burst remaining */
   int cpuLeft2 = x2;                       /* P2 next CPU burst remaining */
   int ioLeft1 = y1;        /* P1 next IO burst remaining, 0 if no more IO */
   int ioLeft2 = y2;        /* P2 next IO burst remaining, 0 if no more IO */
   int qleft;                                         /* quantum remaining */

   for (i=0; (state1 != DONE) || (state2 != DONE); i++) {
                                /* running process completes its CPU burst */
      if ((state1 == RUNNING) && (cpuLeft1== 0)) {
                                                         
         if (ioLeft1 == 0) {
            state1 = DONE;
            s1[i] = stateChars[state1];            /* terminate the string */
         }
         else
            state1 = WAITING;
      }  
      else if ((state2 == RUNNING) && (cpuLeft2 == 0)) {
         if (ioLeft2 == 0){
            state2 = DONE;
            s2[i] = stateChars[state2];
         }
	 else
            state2 = WAITING;
      }  
                                    /* running process has quantum expire */
      if ((state1 == RUNNING) && (qleft == 0) ) {
         state1 = READY;
      }  
      if ((state2 == RUNNING) && (qleft == 0) ) {
         state2 = READY;
      }  
                                                     /* handle IO complete */
      if ((state1 == WAITING) && (ioLeft1 == 0)) {
         state1 = READY;
         cpuLeft1 = z1;
         
      }  
      if ((state2 == WAITING) && (ioLeft2 == 0)) {
         state2 = READY;
         cpuLeft2 = z2;
      }

      if ((state1 == RUNNING) && (cpuLeft2 < cpuLeft1) && (cpuLeft2 != 0)){
         state2 = RUNNING;
         state1 = READY;
      }
      if ((state2 == RUNNING) && (cpuLeft1 <= cpuLeft2) && (cpuLeft1 != 0)){
         state1 = RUNNING;
         state2 = READY;
      }  
                                    /* if both ready, depends on algorithm */
      if ( (state1 == READY) && (state2 == READY) ) {
         if ( cpuLeft1 < cpuLeft2 ) {
            state1 = RUNNING;
         } else if ( cpuLeft1 > cpuLeft2 ) {
            state2 = RUNNING;
         } else {
            state1 = RUNNING;
         }
      }  
                                      /* handle one ready and CPU available */
      else if ( (state1 == READY) && (state2 != RUNNING)) {
         state1 = RUNNING;
      }  
      else if ( (state2 == READY) && (state1 != RUNNING)) {
         state2 = RUNNING;
         
      }  
   /* insert chars in string, but avoid putting in extra string terminators */
      if (state1 != DONE)
         s1[i] = stateChars[state1];
      if (state2 != DONE)
         s2[i] = stateChars[state2];
                                                        /* decrement counts */
      qleft--;                   /* OK to decrement even if nothing running */
      if (state1 == RUNNING)
         cpuLeft1--;
      if (state1 == WAITING)
         ioLeft1--;
      if (state2 == RUNNING)
         cpuLeft2--;
      if (state2 == WAITING)
         ioLeft2--;
   }                                               /* end of main for loop */     
 
}
/******************** rr *****************************************************
    void rr(char *s1,char *s2,int x1,int y1,int z1,int x2,int y2,int z2){
Purpose:
    
Parameters:
    O  char* s1, s2                   
    O  int   quantum, x1,y1,z1,x2,y2,z2                
Returns:
    n/a
Notes:
    Used Korkmaz's proto method to build this function
*******************************************************************************/
void rr(char *s1, char *s2, int quantum, int x1, int y1, int z1, int x2, int y2, int z2){
   int i;                                   /* next string position (time) */
   int state1 = READY;                            /* start with both ready */
   int state2 = READY;
   int cpuLeft1 = x1;                       /* P1 next CPU burst remaining */
   int cpuLeft2 = x2;                       /* P2 next CPU burst remaining */
   int ioLeft1 = y1;        /* P1 next IO burst remaining, 0 if no more IO */
   int ioLeft2 = y2;        /* P2 next IO burst remaining, 0 if no more IO */
   int qleft;                                         /* quantum remaining */

   for (i=0; (state1 != DONE) || (state2 != DONE); i++) {
                                /* running process completes its CPU burst */
      if ((state1 == RUNNING) && (cpuLeft1 == 0)) {
                                                         
         if (ioLeft1 == 0) {
            state1 = DONE;
            s1[i] = stateChars[state1];            /* terminate the string */
         }
         else
            state1 = WAITING;
      }  
      else if ((state2 == RUNNING) && (cpuLeft2 == 0)) {
         if (ioLeft2 == 0){
            state2 = DONE;
            s2[i] = stateChars[state2];
         }
	 else
            state2 = WAITING;
      }  
                                     /* running process has quantum expire */
      if ((state1 == RUNNING) && (qleft == 0) ) {
         state1 = READY;
         if (state2 == READY) {
           state2 = RUNNING;
           qleft = quantum;
         }         
      }  
      if ((state2 == RUNNING) && (qleft == 0) ) {
         state2 = READY;
         if (state1 == READY) {
           state1 = RUNNING;
           qleft = quantum;
         }
      }  
                                                     /* handle IO complete */
      if ((state1 == WAITING) && (ioLeft1 == 0)) {
         state1 = READY;
         cpuLeft1 = z1;
         
      }  
      if ((state2 == WAITING) && (ioLeft2 == 0)) {
         state2 = READY;
         cpuLeft2 = z2;
      } 
                                    /* if both ready, depends on algorithm */
      if ( (state1 == READY) && (state2 == READY) ) {
         state1 = RUNNING;
         qleft = quantum;
      }  
                                     /* handle one ready and CPU available */
      else if ((state1 == READY) && (state2 != RUNNING)) {
         state1 = RUNNING;
         qleft = quantum;
         
      }  
      else if ((state2 == READY) && (state1 != RUNNING)) {
         state2 = RUNNING;
         qleft = quantum;
      }  

   /* insert chars in string, but avoid putting in extra string terminators */
      if (state1 != DONE)
         s1[i] = stateChars[state1];
      if (state2 != DONE)
         s2[i] = stateChars[state2];
                                                        /* decrement counts */
      qleft--;                   /* OK to decrement even if nothing running */
      if (state1 == RUNNING)
         cpuLeft1--;
      if (state1 == WAITING)
         ioLeft1--;
      if (state2 == RUNNING)
         cpuLeft2--;
      if (state2 == WAITING)
         ioLeft2--;
   }                                                /* end of main for loop */     
     
} 
