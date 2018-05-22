#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include "nqueens_library.h"
/*************************generate_n_permutations************************ 
    int generate_n_permutations(int n, int print_flag)
Purpose:
    This function generates the permutations and returns the amount found. 
    If the flag is true, the permutations will be printed to stdout
Parameters:
    O  int	n
	O  int 	print_flag
Returns:
	solutions 
Notes:
************************************************************************/
int generate_n_permutations(int n, int print_flag){
   char *string;
   int list[n];
   int size; 
   int i;
   int p = 1;
   unsigned long solutions = 1;
   string = (char *)malloc((size+1) * sizeof(char));
   size = n * 4;
   for(i = 0; i < n; i++){
        list[i] = i;
    }
   if(print_flag){
      get_array_string(list,n,string);
      fprintf(stdout, "%s", string);
   }
   if(n > 1){  
      while(next_permutation(list, n) != 1){
         p++;
         solutions++;
         if(print_flag){
            get_array_string(list,n,string);
            fprintf(stdout, "%s", string);
         }
         if(n == 1) 
         break;  
       }  
   }
   //printf("Total amount of permutations: %d \n", p);
   free(string);
   return solutions;
}


/*************************generate_n_queens_serial***************************** 
    int generate_n_queens_serial(int n, int print_flag)
Purpose:
   This function generates and returns the solutions to the n-queens puzzle.
   Will print to stdout if print flag is true
Parameters:
    O  int	n
	O  int 	print_flag
Returns:
	solutions
Notes:
************************************************************************/
int generate_n_queens_serial(int n, int print_flag){
   char *string;
   int list[n];
   int size; 
   int i;
   unsigned long solutions = 0;
   string = (char *)malloc((size+1) * sizeof(char));
   size = n * 4;
   for(i = 0; i < n; i++){
        list[i] = i;
    }

   while(next_permutation(list, n) != 1){
      if(check_diagonal(list,n)){
         solutions++;
         if(print_flag){
            get_array_string(list,n,string);
            fprintf(stdout, "%s", string);
         }
      }
      if(n == 1) 
      break;  
    }  
   
   free(string);
   return solutions;
}


/****************************get_array_string**********************************
    void get_array_string(int *list, int n, char *s)
Purpose:
	Fills the string with parameter with the string containing
	n ints in the list
Parameters:
    O  int *list
	O  int n
	O  char *s
Returns:
    N/A
Notes:
******************************************************************************/
void get_array_string(int *list, int n, char *s){
    int i;
    for(i = 0; i < n; i++){
        sprintf(s,"%4d", list[i]);
        s += 4;
    }
    sprintf(s,"\n");
}


/*************************next_permutation*************************************
    int next_permutation(int *list, int n)
Purpose:
	Generates the next permutation
Parameters:
    O  int *list
	O  int n
Returns:
    1 
	0 
Notes:
******************************************************************************/
int next_permutation(int *list, int n){
   int i; 
   int k = -1; 
   int l = -1; 
   int temp;   
   if(n == 1){
      return 0;
   }
  
   for(i = 0; i < (n-1); i++){
      if(list[i+1] > list[i]){ 
         k = i;
      }
   }

   if(k == -1){
      return 1;
   }
   
   for(i = k+1; i < n; i++){
      if(list[i] > list[k]){
         l = i;
      }
   }
   temp = list[k];
   list[k] = list[l];
   list[l] = temp;
   
   l = n-1;
   for(i = k+1; i < n; i++){
      temp = list[i];
      list[i] = list[l];
      list[l] = temp;
      l--;
      if(i >= l){
         break;
      }
   }

   return 0;
}
/****************************check_diagonal************************************
    int check_diagonal(int *list, int n)
Purpose:
    checks if the list satisfies diagonal property
Parameters:
    O  int *list
	O  int n
Returns:
    1 
	0 
Notes:
******************************************************************************/
int check_diagonal(int *list, int n){
   int i; 
   int j; 
   int temp; 
   for(i = 0; i < n; i++){
      for( j = i+1; j < n; j++){
         temp = list[i] < list[j] ? (list[j] - list[i]) : (list[i] - list[j]);
         if(temp == j - i){
            return 0;
         }
      }
   }
   return 1;
}

/**********************generate_n_queens_serial_one****************************
    int generate_n_queens_serial_one(int n, int out_flag, int first)
Purpose:
	generates all solutions to the n-queens problems whose first value is first 
Parameters:
    O  int n
	O  int out_flag
	O  int first
Returns:
	solutions
Notes:
******************************************************************************/
int generate_n_queens_serial_one(int n, int out_flag, int first){
   char *string;
   int list[n];
   int size; 
   int i; 
   int hit = 0;
   unsigned long solutions = 0;
   size = n * 4;
   string = (char *)malloc((size+1) * sizeof(char));
   for(i = 0; i < n; i++){
      list[i] = i;
   }
   
   while(next_permutation(list, n) != 1){
      if(list[0] != first){
         if(hit == 0)
            continue;
         else
            break;
      }
      hit = 1;
      if(check_diagonal(list,n)){
         solutions++;
      }
      
      if(n == 1) break;  
    }  
   
   free(string);
   return solutions;
}

/********************************queens_thread*********************************
   void *queens_thread(void *infopointer)
Purpose:
    Sets a page in the page table. 
Parameters:
    O  unsigned long	physicalPage
	O  unsinged long 	virtualPage
Returns:
    1 on success
	0 on failure
Notes:
******************************************************************************/
void *queens_thread(void *infopointer){
	int result = generate_n_queens_serial_one(((struct ti*)infopointer)->n,((struct ti*)infopointer)->out_flag,((struct ti*)infopointer)->first);
	((struct ti*)infopointer)->result = result;
	printf("Value of first = %d Solutions = %d\n",((struct ti*)infopointer)->first, result);
	return NULL;
}

void *queens_thread2(void *infopointer){
   thread_info_t * th = (thread_info_t *)(infopointer);
   th->result = generate_n_queens_serial_one(th->n, th->out_flag, th->first);
   printf( "Value of first = %d Solutions =  %d\n", th->first, th->result);
   pthread_exit((void *) infopointer);
   
}



