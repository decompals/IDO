/* 
 *   sem_op2.c:
 *
 *     This set of programs (sem_op1 and sem_op2) illustrates a simple 
 *  implementation of semaphore operations.  To demonstrate, first 
 *  execute sem_op1 (in the background--i.e. "sem_op1 &"--or from another
 *  window/shell, and then execute sem_op2.
 *
 *  References:  INTRO(2), SEMGET(2), SEMOP(2), SEMCTL(2), SIGNAL(2)
 *
 *                                     George Smith - 1987
 */

#include <stdio.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#define SEMKEY                10  /* Why not ? */
#define Program                "sem_op2"

/* 
 *     This is the declaration as it appears in sem.h:
 *
 *  struct sembuf {
 *      ushort  sem_num;        / semaphore # /
 *      short   sem_op;         / semaphore operation /
 *      short   sem_flg;        / operation flags /
 *  };
 *
 */

struct sembuf sem_ar;
int  sig_array[2];
int out_val[2];
int i;
unsigned int count;
int semid;
int val, num;


main () {

        void cleanup();
/* Open semaphore */
        if( ( semid=semget(SEMKEY,2,0777|IPC_CREAT)) == -1) {
                printf("\n Can't open semaphor ( %s ) \n",Program);
                fflush(stdout);
                exit(-1);
        }

/* Catch signals */
        for(i=1; i<21 ; i++ ) 
                signal(i , cleanup);

        

/* Get the value of the semaphores and print */

        for( i=0 ; i<2 ; i++ ) {
                if( (val =semctl(semid, i , GETVAL, out_val )) == -1 ){
                        printf("\n Can't read semval ( %s ) \n",Program);
                        fflush(stdout);
                        exit(-1);
                }
                printf("\n Semval [%d]: %d ",i,val );
                fflush(stdout);
        }

        printf("\n");
        fflush(stdout);

/* Incremewnt 0  */
        sem_ar.sem_num = 0 ;
        sem_ar.sem_op = 1;
        sem_ar.sem_flg = SEM_UNDO ;
        if( semop(semid, &sem_ar , 1 ) == -1 ) {
                printf("\n Can't do semop ( %s ) " ,Program );
                fflush(stdout);
                cleanup();
        }
        
/* sleep then die */
        sleep(1);
        cleanup();
}


void cleanup() {

        semctl(semid, 2 , IPC_RMID,0 );
        exit(1) ;
}        
