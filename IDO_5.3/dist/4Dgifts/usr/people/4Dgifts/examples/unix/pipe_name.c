/* 
 *   pipe_name.c:
 *
 *    Program to illustrate named pipes 
 *
 *  References:  UNLINK(2), MKNOD(2), FORK(2), OPEN(2), WAIT(2), READ(2),
 *               WRITE(2
 *
 *                                     George Smith - 1987
 */

#include <stdio.h>
#include <fcntl.h>

int fd[2] ;
char w_strng[] = {" A test of a Named Pipe "};
char r_strng[80];
int pid, nc, status;

main () {


/* First Make a node--this could have already been done by use of mknod(1) */
        /* Unlink node if already exists */
        unlink("Node");
        mknod("Node", 0010644,0);

/* Now fork--these proccesses need not be related to use named pipes */

        if( (pid=fork())) {      /* Must be Parent */

                fd[0] = open("Node" , O_RDONLY );
                wait(&status);
                nc = read( fd[0] , r_strng, sizeof(w_strng)) ;
                printf("\n Child died and now I have read (%d) char from pipe",
                                                           nc);
                printf("\n ( %s ) \n", r_strng);
                exit(0);

        } else {                 /* Must be Child */

                fd[0] = open("Node", O_WRONLY );
                nc= write(fd[0],w_strng,sizeof(w_strng));
                printf("\n Child Here--Just wrote %d char to pipe \n", nc);
                sleep(2);        
                exit(0);
        }
}
