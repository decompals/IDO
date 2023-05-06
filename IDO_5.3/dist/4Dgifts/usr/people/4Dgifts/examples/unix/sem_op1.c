/*
 *   sem_op1.c:
 *
 *    This set of programs (sem_op1 and sem_op2) illustrates a simple
 *  implementation of semaphore operations.  To demonstrate, first 
 *  execute sem_op1 (in the background ("sem_op1 &"), or from another
 *  window/shell), and then execute sem_op2.
 *
 *  References:  INTRO(2), SEMGET(2), SEMOP(2), SEMCTL(2), SIGNAL(2)
 *
 *                                     George Smith - 1987
 *
 */

#include <stdio.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#define SEMKEY                10  /* Why not ? */
#define Program               "sem_op1"

/*
 *      This is the declaration as it appears in sem.h:
 *  struct sembuf {
 *      ushort  sem_num;        / semaphore # /
 *      short   sem_op;         / semaphore operation /
 *      short   sem_flg;        / operation flags /
 *  };
 */

struct sembuf sem_ar;
int  sig_array[2];
int i, cleanup();
unsigned int count;
int semid;


main ()
{

/* Open semaphore */
        if( ( semid=semget(SEMKEY,2,0777|IPC_CREAT)) == -1) {
                printf("\n Can't open semaphor ( %s ) \n",Program);
                fflush(stdout);
                exit(-1);
        }

/* Catch signals */
/*      for(i=1; i<21 ; i++ ) 
                signal(i , cleanup );
*/

        
/* Set up initial value of 1 , 1 */
        sig_array[0] = sig_array[1] = 1 ;

        if( semctl(semid,2,SETALL,sig_array) == -1 ) {
                printf("\n Can't do semctl (%s)\n",Program);
                fflush(stdout);
                cleanup();
        }

/* Pause until sig_array[0] increments  */
        sem_ar.sem_num = 0 ;
        sem_ar.sem_op = -1 ;
        sem_ar.sem_flg = SEM_UNDO ;

        if(semop(semid, &sem_ar , 1 ) == -1 ) {
                printf("\n Can't do semop ( %s ) \n" , Program );
                fflush(stdout);
                cleanup() ;
        }


        printf("\n **> Was waiting on Increment , Now i am awake ( %s )\n",
                                                                Program);
        fflush(stdout);

/* Remove semid from system */

        cleanup();
}


cleanup() {

        semctl(semid, 2 , IPC_RMID,0 );
        exit(1) ;
}        
