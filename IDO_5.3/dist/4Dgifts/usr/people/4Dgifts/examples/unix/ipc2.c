/*
 *   ipc2.c:
 *
 *     Send and receive messages via inter-process communication (IPC).
 *  This program works in conjunction with ipc1.  
 *     ipc2 catches signals and calls cleanup to remove the message queue 
 *  from the system. 
 *     To run, execute ipc2 in the background 1st and then run ipc1 (i.e.
 *  "ipc2 &; ipc1").
 *
 *  References:  INTRO(2), MSGCTL(2), MSGGET(2), MSGOP(2), SIGNAL(2)
 *
 *                                     George Smith - 1987
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/signal.h>

#define MSGKEY       100         /* Key = 100--see msgget(2) */
#define MTYPE_SEND     2         /* Send type */
#define MTYPE_RECV     1         /* Receive type */
#define Program    "ipc2"        /* Program name */

int i;
int msg_id;

struct msgstrct {
    long mtype;                  /* message type */
    char text[256] ;             /* message text; here size is in 256 bytes */
} msg;


main() {

        int pid ;
        void cleanup();

        if( (msg_id= msgget(MSGKEY,0777 | IPC_CREAT)) == -1 ) {
                printf("\n Error opening msgqueue \n");
                fflush(stdout);
                exit(-1);
        }
/* Catch signals */
        for( i=1 ; i<21 ; i++ )
                signal(i, cleanup);

/* Send pid in message */
        pid = getpid() ;

/* Send message to other msg program */
        sprintf(msg.text," From %6d (%s): I'm ipc2: Hello Y'all",pid, Program);
        
/* Set message type */
        msg.mtype = MTYPE_SEND  ;

        
/* Send message */
        if( msgsnd(msg_id, &msg, sizeof(msg), 0 ) == -1 ) {
                printf("\n Can't send message : from (%s) \n " ,Program);
                fflush(stdout);
        
                cleanup();
                exit(-1);
        }
/* Receive message */
        if( msgrcv(msg_id, &msg , sizeof(msg), MTYPE_RECV , 0 ) == -1){
                printf("\n Can't recieve message : from ( %s )\n",Program );
                cleanup() ;
                exit(-1);
        }

/* Print message and type */
        printf("\n Message received by (%s)\n Message follows:\n (%s) \n",
				      Program,                     msg.text );
        fflush(stdout);

/* Sleep to make sure other program gets a chance */
        sleep(20) ;
/* In case we didn't get kill ; lets cleanup and die */
        cleanup() ;
        exit(0) ;
}


void cleanup() {

        msgctl(msg_id , IPC_RMID , 0 );
        exit(0) ;
}
