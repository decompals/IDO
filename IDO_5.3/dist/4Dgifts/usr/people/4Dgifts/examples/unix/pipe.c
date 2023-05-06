/*        
 *   pipe.c:
 *
 *    Demonstrates use of unnamed pipes.
 *
 *  References: PIPE(2), FORK(2), WAIT(2), READ(2), WRITE(2)
 *
 *                                     George Smith - 1987 
 */

#include <stdio.h>

int fd[2] ;
char w_strng[]= { " This is a test of the Pipe" };
char  r_strng[80];
int pid ,status;

main () {

/* Make pipe channels ; fd[0] = read pipe ; fd [1] = write pipe */

        pipe(fd);

        if( (pid=fork()) ) {         /* Must be parent */

                write(fd[1],w_strng,sizeof(w_strng));
                wait(&status);
                printf("\n Bye Bye \n");
                fflush(stdout)        ;

        } else {                    /* Must be Child */

                read(fd[0],r_strng,sizeof(w_strng));
                printf("\n Go it: \n ( %s )", r_strng);
                exit(0);
        }
}
