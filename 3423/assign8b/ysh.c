#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>


struct cmd
  {
    int redirect_in;     /* Any stdin redirection?         */
    int redirect_out;    /* Any stdout redirection?        */
    int redirect_append; /* Append stdout redirection?     */
    int background;      /* Put process in background?     */
    int piping;          /* Pipe prog1 into prog2?         */
    char *infile;        /* Name of stdin redirect file    */
    char *outfile;       /* Name of stdout redirect file   */
    char *argv1[10];     /* First program to execute       */
    char *argv2[10];     /* Second program in pipe         */
  };


int cmdscan(char *cmdbuf,struct cmd *com);


int main(){
        int fd[2];
        int fdin;
        int fdout;
        char buf[1024];
        pid_t ppid;
        pid_t cpid;
        struct cmd command;
        printf(">");
        fflush(stdout);

        while ((fgets (buf, sizeof(buf),stdin) != NULL)){
                if (buf[strlen(buf) -1] == '\n'){
                        buf[strlen(buf) -1] = '\0';
                }
                if (cmdscan(buf, &command) || (strcmp(buf, "exit") == 0)){
                        exit(-1);
                }
                if (pipe(fd) <0){
                        perror("pipe error");
                        exit(-1);
                }
                switch (ppid = fork()){
                        case -1:
                                perror("fork error");
                                exit(-1);
                        case 0://child
                                if (command.piping){
                                        switch(cpid = fork()){
                                                case -1:
                                                        perror("fork error");
                                                        exit(-1);
                                                case 0://grandchild
                                                        dup2(fd[1], STDOUT_FILENO);
                                                        close(fd[1]);
                                                        close(fd[2]);
                                                        if (command.redirect_in){
                                                                if ((fdin = open(command.infile,O_RDONLY) <0)){
                                                                                perror("open error on infile");
                                                                                exit(-1);
                                                                }
                                                                dup2(fdin, STDIN_FILENO);
                                                                close(fdin);
                                                        }
							// executing prog1 as gchild
                                                        execvp(command.argv1[0],command.argv1);
                                                        perror("prog1 exec error");
                                                        exit(-1);
                                                default://child
                                                        dup2(fd[0], STDIN_FILENO);
                                                        close (fd[0]);
                                                        if (command.redirect_out){
                                                                if (command.redirect_append){
                                                                        if((fdout = open(command.outfile, O_RDWR| O_CREAT| O_APPEND, S_IRUSR| S_IWUSR)) < 0){
                                                                                perror("open error in redirect append");
                                                                                exit(-1);
                                                                        }
                                                                }else{
                                                                        if ((fdout = open(command.outfile, O_RDWR| O_CREAT| O_TRUNC, S_IRUSR| S_IWUSR)) < 0){
                                                                                perror("open error on redirect truncate");
                                                                                exit(-1);
                                                                        }
                                                                }
                                                                dup2(fdout, STDOUT_FILENO);
                                                                close(fdout);
                                                                }
                                                                close(fd[1]);
								// executing prog2 as original child
                                                                execvp(command.argv2[0],command.argv2);
                                                                perror("prog2 exec error");
                                                                exit(-1);
                                                                if (!command.background){
                                                                        if(waitpid (cpid,NULL,0) != cpid){
                                                                                perror("error on background");
                                                                                exit(-1);
                                                                        }
                                                                }
                                                        }
                                        }else{
                                                if(command.redirect_out){
                                                        if (command.redirect_append){
                                                                if ((fd[1] = open(command.outfile, O_RDWR | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR)) < 0){
                                                                        perror("open redirect append error");
                                                                        exit(-1);
                                                                }
                                                        }else{
                                                                if ((fd[1] = open(command.outfile, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR)) < 0 ){
                                                                        perror("open redirect trunc error");
                                                                        exit(-1);
                                                                        }
                                                                }
                                                        dup2(fd[1], STDOUT_FILENO);
                                                        close (fd[1]);
                                                }
                                                if (command.redirect_in){
                                                        if (( fd[0] = open (command.infile, O_RDONLY)) < 0){
                                                                perror("open infile error");
                                                                exit(-1);
                                                        }
                                                        dup2(fd[0], STDIN_FILENO);
                                                        close (fd[0]);
                                                }
                                                execvp(command.argv1[0],command.argv1);
                                                perror("prog1 exec error");
                                                exit(-1);
                                                if (command.redirect_in){
                                                        close(STDIN_FILENO);
                                                }
					}
				}
                                 printf(">");
                                 fflush(stdout);
                        }
        exit(0);
}

