/*
 *   child.c:
 *
 *    Fork a child and then kill it .  This programs forks a child then after
 *  sleeping sends it 2 signals, the second of which kills it.
 *
 *  References:  INTRO(2), SIGNAL(2), FORK(2), KILL(2)
 *
 *                                     George Smith - 1987
 */

#include <stdio.h>
#include <sys/signal.h>

int i,j,pid;
char tmp_strng[80];

main() {

        void do_it();

        i=0;
        printf("\n Forking child \n");
        fflush(stdout);
        
        pid=fork();
        if( pid == 0 ) {
                printf("\n Child HERE :::: \n");
                fflush(stdout);
                signal(SIGUSR1,do_it);
            
                sleep(60);
                printf("\n ******* Child is Awake ********* \n");
                fflush(        stdout);
                sleep(60);
        } else {
                sleep(3);
                printf("\n pid (%d) Parent will now signal child \n",pid);
                fflush(stdout);
                kill(pid,SIGUSR1);
                sleep(5);
                printf("\n pid (%d) Parent will now KILL signal child \n",pid);
                fflush(stdout);
                kill(pid,SIGUSR1);
        }
}


void do_it(sig) 
int sig;
{
        i++;
        signal(SIGUSR1,do_it);                             /* Reset signal */
        
        printf("\n Got Signal (%d) I= (%d)\n",sig,i);
        if( i >1 ) {
                printf("\n Child is going to die now \n");
                fflush(stdout);
                exit(0);
        }
        return ;
}
