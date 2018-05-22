#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>



int main(void){

	int process = 10;
	int i, j;
	int parent;
	int fd[process][2];
	int x = 0;
	int n; 
	int rng;
	int num;
	char buf[9];
	char buff[9];
	
	for (i = 0; i < process ; i++){
		if (pipe(fd[i])){
			perror("Pipe Error");
			exit(-1);
		}
	}

	for (i = 1; i< process; i++){
		switch(parent = fork()){
			case -1:
				perror("Fork Error");
				exit(-1);
			case 0:
				x = i;
				break;
		}
		if (parent){
			break;
		}
	}

	for ( i = 0; i < process; i++){
		if(i != x){
			close(fd[i][0]);
		}
		else{
			close (fd[i][1]);
		}
	}


	rng = RAND_MAX/process;
	rng *= process;
	srand(1921739 * i);
	for (i = 0; i < 9; i++){
		do{
			num = rand();
			j = num % process;
		}
		while ( num >= rng || x == j);

		//char buf[9];
		snprintf(buf, 9 ,"process%d", x);
		if (write (fd[j][1],buf,sizeof(buf)) <0){
			perror("Write Error");
			exit(-1);
		}
	}

	for (i = 0; i < process; i++){
		if (x != i){
			close(fd[i][1]);
		}
	}

	while (read( fd[x][0], buff, sizeof(buff)) > 0){
		fprintf(stdout,"process%d has received a message from %s\n",x , buff);
	}

	close(fd[x][0]);
	exit(0);
}

