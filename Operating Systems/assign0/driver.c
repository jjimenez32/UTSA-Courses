#include <stdio.h>
#include <stdlib.h>
#include "myio.h"
int main(int argc, char *argv[])
{
   int i;         // hold the integer returned from ReadInteger()
   double d;      // holds the double returned from ReadDouble()
   char *s;       // holds the string returned by ReadLine()
   int ix;        // variable for amount of ReadInteger() calls
   int iy;        // variable for amount of ReadDouble() calls
   int iz;        // variable for amount of ReadLine() calls
   FILE *fp;      // file pointer for output file
   
   //following is to check for correct amount of arguments
   if (argc < 5){
      fprintf(stderr, "Not enough arguments\nProper method: assign0 x y z output_filename\n");
      exit(1);
   } else if(argc > 5){	
      fprintf(stderr, "Too many arguments\nProper method: assign0 x y z output_filename\n");
      exit(1);
   }else {
      ix = atoi(argv[1]);
      iy = atoi(argv[2]);
      iz = atoi(argv[3]);
      fp = fopen(argv[4],"w");
   }
   //call the Read functions depending on amount of x y z 
   while (ix > 0){
      printf("Enter Integer: ");
      i = ReadInteger();
      //atoi would return 0 if not an integer
      if (i != 0){
         fprintf(fp,"Integer:%i\n",i);
         ix--;
      } else{
         printf("Not a valide entry for an Integer, please re-enter\n");
         continue;
      }
   }
   while (iy > 0){
      printf("Enter Double: ");
      d = ReadDouble();
      //atof would return 0 if not an integer
      if (d != 0){
         fprintf(fp,"Double:%lf\n", d);
         iy--;
      } else {
         printf("Not a valid entry for a Double, please re-enter\n");
         continue;
      }
   }
   while (iz > 0){
      printf("Enter String:");
      s = ReadLine();
      fprintf(fp, "String:%s", s);
      iz--;
   }
   //close the file pointer
   fclose(fp);
   exit(0);
}

