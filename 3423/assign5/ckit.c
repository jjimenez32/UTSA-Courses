#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#define BUFSZ 1024
int main (int argc, char *argv[]){
	char buf[BUFSZ];
	int x;
	int file1;
	int file2;
	int fileout;
	//Checking if argument amount is < 3
	if (argc != 3){
		if (argc != 4){
			write(STDERR_FILENO,"ERROR: Argument amount\n",23);
			exit(-1);}
	}
	// Invocations for 2 arguments
	if (argc == 3){
		//Checking if arguments 1 and 2 are "-"
		if ((strcmp (argv[1],"-") == 0 && (strcmp(argv[2],"-") ==0))){
			write(STDERR_FILENO,"ERROR\n",6);
			exit(-1);}
		//Writing from STDIN to STDOUT if argument is "-"
		if (strcmp (argv[1], "-") == 0){
			file1 = STDIN_FILENO;
			if ((file2 = open(argv[2], O_RDONLY)) <0){
				perror(argv[2]);
				close(file1);
				exit(-1);}
			while ((x = read(file1,buf,BUFSZ)) >0){
				write(STDOUT_FILENO,buf,x);}
			while ((x = read(file2,buf,BUFSZ)) >0){
				write(STDOUT_FILENO,buf,x);}
			close(file1);
			close(file2);
		}
		//Writing from STDIN to STDOUT if argument is "-"
		if (strcmp (argv[2], "-") ==0){
			file2 = STDIN_FILENO;
			if ((file1 = open(argv[1], O_RDONLY)) <0){
				perror(argv[1]);
				exit(-1);}
			while ((x = read(file1,buf,BUFSZ))>0){
				write(STDOUT_FILENO,buf,x);}
			while ((x = read(file2,buf,BUFSZ))>0){
				write(STDOUT_FILENO,buf,x);}
			close(file1);
			close(file2);
		}
		// Writing to STDOUT if both are not "-"
		if ((strcmp (argv[1],"-") != 0) && (strcmp(argv[2],"-")!=0)) {
			if ((file1 = open(argv[1], O_RDONLY)) <0){
				perror(argv[1]);
				exit(-1);}
			if ((file2 = open(argv[2], O_RDONLY)) <0){
				perror(argv[2]);
				close(file1);
				exit(-1);}
			while ((x = read(file1,buf,BUFSZ))>0){
				write(STDOUT_FILENO,buf,x);}
			while ((x = read(file2,buf,BUFSZ))>0){
				write(STDOUT_FILENO,buf,x);}
			close(file1);
			close(file2);
		}
	}
	//invocations for 3 arguments
	if (argc == 4) {
		//Checking if both arguments are "-"
		if ((strcmp (argv[1], "-") == 0) && (strcmp(argv[2],"-") == 0)){
			write(STDERR_FILENO,"ERROR\n",6);
			exit(-1);}
		//Writing from STDIN to fileout if argument is "-"
		if (strcmp (argv[1],"-") ==0){
			file1 = STDIN_FILENO;
			if ((file2 = open(argv[2], O_RDONLY))<0){
				perror(argv[2]);
				close(file1);
				exit(-1);}
			if ((fileout = open(argv[3], O_WRONLY|O_TRUNC|O_CREAT, 0600)) <0){
				perror(argv[3]);
				close(file1);
				close(file2);
				exit(-1);}
			while((x = read(file1,buf,BUFSZ))>0){
				write(fileout,buf,x);}
			while((x = read(file2 ,buf,BUFSZ))>0){
				write(fileout,buf,x);}
			close(file1);
			close(file2);
			close(fileout);}
		//Writing from STDIN to fileout if argument is "-"
		if (strcmp (argv[2],"-") == 0){
			file2 = STDIN_FILENO;
			if ((file1 = open(argv[1], O_RDONLY)) <0){
				perror(argv[1]);
				exit(-1);}
			if ((fileout = open(argv[3], O_WRONLY|O_TRUNC|O_CREAT,0600)) <0){
				perror(argv[3]);
				close(file1);
				close(file2);
				exit(-1);}			
			while ((x = read(file1,buf,BUFSZ))>0){
				write(fileout,buf,x);}
			while ((x = read(file2,buf,BUFSZ))>0){
				write(fileout,buf,x);}
			close(file1);
			close(file2);
			close(fileout);
		}
		//Writing to fileout if both are not "-"
		if ((strcmp(argv[1],"-")!=0) && (strcmp(argv[2],"-") != 0)){
			if ((file1 = open(argv[1], O_RDONLY)) <0){
				perror(argv[1]);
				exit(-1);}
			if ((file2 = open(argv[2], O_RDONLY)) <0){
				perror(argv[2]);
				close(file1);
				exit(-1);}
			if ((fileout = open(argv[3],O_WRONLY|O_TRUNC|O_CREAT, 0600)) <0){
				perror(argv[3]);
				close(file1);
				close(file2);
				exit(-1);}
			while ((x = read(file1,buf,BUFSZ)) >0){
				write(fileout,buf,x);}
			while ((x = read(file2,buf,BUFSZ)) >0){
				write(fileout,buf,x);}
			close(file1);
			close(file2);
			close(fileout);
		}
	}
	return 0;
}
