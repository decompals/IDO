/*
 *   shmem1.c:
 *
 *    This set of programs (shmem1 and shmem2) demonstrates creating and 
 *  then sending/receiving data to/from a shared memory segment.  shmem1 
 *  reads from the shared memory segment and prints its contents to the 
 *  shell window shmem1 was executed from.
 *    To run this set, execute shmem1 first (in the background ("shmem1 &")
 *  or from another shell/window), then run shmem2.  To exit, hit Ctrl-C for
 *  shmem2, then bring shmem1 back into the foreground (fg) and hit Ctrl-C 
 *  for it as well.  Notice the use of signal(SIGINT, bye) (below) to catch 
 *  the Ctrl-C and then exit after performing system cleanup by 
 *  de-allocating the shared memory segment.
 *
 *  References:  INTRO(2), SIGNAL(2), SHMGET(2), SHMOP(2), SHMCTL(2)
 */

#include <signal.h>
#include "shmdefs.h"

int shmid;


main()
{
        volatile char *buf;     /* Pointer to shared memory. Must be volatile */
				/* to prevent optimizations which would keep */
				/* dereferenced values in registers instead */
				/* of loading from shared memory each time */
	void  bye();


	signal(SIGINT,bye);	/* whenever a ^C is typed go to exit 
				   subroutine bye() */
				/* first allocate a shared memory segtment */
	if((shmid = shmget(5,1024,0770 | IPC_CREAT)) < 0) {
		perror("shmget");
		exit(0);
	}
				/* now attach to the shared memory segment */
	if((long) (buf = shmat(shmid, 0, 0)) < 0) {
		perror("shmat");
		exit(0);
	}
				/* now access shared memory segment */
				/* as you would a normal buffer */
	*buf = '\0';

	while(1) {
	   while(*buf != '\0') {
		/* print message from another process */
	      printf("\"I'm\" shmem1:  shmem contains: \"%s\"\n",buf);
	      *buf = '\0';
	   }
 	}
}


void bye()	/* here is where the shared memory segment is deleted */
{
	if((shmctl(shmid,IPC_RMID)) < 0) {
		perror("shmctl");
		exit(0);
	}

	printf("exiting normally\n");
	exit(1);
}
