#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "pslibrary.h"
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
    fills string with appropriate characters r, R , and w depending on the int 
    parameters passed in
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
   /* The first string, s1, will consist of a x1 R's, followed by y1 w's,
      followed by 0 or more r's, followed by z1 R's.             
      The second string, s2, will be consist of x1 r's, followed by x2 R's,
      followed by y2 w's, followed by 0 or more r's, followed by z2 R's.*/
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
