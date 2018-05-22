#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "myio.h"
/**************************ReadInteger*****************************************
	int ReadInteger();
Purpose: Calls ReadLine() function to read string and convert to an Integer
         with atoi()
Returns: char *iInput     the converted int from the string input
Notes: This frees the input string passed in.
******************************************************************************/
int ReadInteger(void){
   int iInput;                  // int to convert a string to
   int i;                       // iterator
   char *input = ReadLine();    // calls ReadLine() to get user input
   //checking if a char is alpha in string
   for (i = 0; i < strlen(input); i++){
      if(isalpha(input[i])){
         free(input);
         return(0);
      }
   }
   iInput = atoi(input);        // converts input to int
   free(input);                 // frees input array
   return (iInput);
}
/**************************ReadDouble*****************************************
	int ReadDouble();
Purpose: Calls ReadLine() function to read string and convert to a Double
         with atof()
Returns: char *iInput     the converted double from the string input
Notes: This frees the input string passed in.
******************************************************************************/
double ReadDouble(void){
   double dInput;                // int to convert a string to
   int i;                        // iterator
   char *input = ReadLine();     // calls ReadLine() to get user input
   //checking if a char is alpha in string
   for (i = 0; i < strlen(input); i++){
      if(isalpha(input[i])){
         free(input);
         return(0);
      }
   }
   dInput = atof(input);         // converts input to double
   free(input);                  // frees input array
   return(dInput);
}
/**************************ReadLine*******************************************
	int ReadLine();
Purpose: Calls ReadLineFile() to return string from stdin
Returns: char *input     String that was returned from ReturnLineFile(stdin)
Notes: N/A
******************************************************************************/
char *ReadLine(void){
   return (ReadLineFile(stdin));
}
/**************************ReadLineFile****************************************
	int ReadDouble(FILE *infile);
Purpose: Reads from stdin a line and converts to a dynamically allocated and 
         resized buffer of a string. 
Parameters:  FILE *infile       file pointer that was passed in by user 
                                in this case it's stdin.
Returns: char *cString     the converted double from the string input
Notes: N/A
******************************************************************************/
char *ReadLineFile( FILE *infile){
   char *cString;             // dynamically allocated string depeding on input
   int i = 0;                 // iterator
   char *input;               // line ptr read from stdin
   size_t len = 0;            // buffer to be updated by chars in string
   ssize_t read;
   
   input = (char *) malloc (sizeof(char));
   if ((read = getline(&input, &len, infile) == -1)){
      fprintf(stderr,"Memory allocation error");
      exit(1);
   }
   return (input);
}
