/*
 *   child_win.c:
 *
 *     Fork a child (which is another window), and then kill it.  This 
 *  programs forks a child, then after sleeping, sends it 2 signals, the 
 *  second of which kills it. 
 *
 *  References:  INTRO(2), SIGNAL(2), FORK(2), KILL(2)
 *
 *                                     George Smith - 1987
 */

#include <stdio.h>
#include <sys/signal.h>
#include <gl/gl.h>

extern do_it() ;
int i,j,pid;
char tmp_strng[80];
int gid;

main() {

        i=0;
        printf("\n Forking child \n");
                fflush(stdout);
        pid=fork();
        if( pid == 0 ) {
                printf("\n Child HERE :::: \n");
                fflush(stdout);
/* Open Graphics window */
                foreground();
                gid=winopen("CHILD");
                color(BLUE);
                clear();
                ortho2(0.,100.,0.,100.);

                signal(SIGUSR1,do_it);
        
/* Sleep so parent can awaken me */
                sleep(60);
                color(WHITE);
                cmov2(10,20);
                charstr("Child is awake");
                sleep(60);

        } else {

/* Open window for parent */
                foreground();
                gid=winopen("PARENT");
                color(RED);
                clear();
                ortho2(0.,100.,0.,100.);
                color(BLACK);
                cmov2(30,30);
                charstr("I'm the parent");
        
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

do_it(sig) 
int sig;
{
        i++;
        signal(SIGUSR1,do_it);                              /* Reset signal */
        
        printf("\n Got Signal (%d) I=(%d)\n",sig,i);
        if( i >1 ) {
                printf("\n Child is going to die now \n");
                fflush(stdout);
                exit(0);
        }
        return ;
}
