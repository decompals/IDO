/*
 *    shmem2.c:
 *
 *    This program writes to the shared memory segment which shmem1 then
 *  reads from.  To run, execute shmem1 first and then run shmem2. To exit,
 *  Ctrl-C out of shmem2 first and then do the same with shmem1.
 *
 *  References:  INTRO(2), SHMGET(2), SHMOP(2), STRING(3C)
 */

#include "shmdefs.h"

char *stuff = "this was written by shmem2 (the other goon)";
int shmid;

/* this program writes to the shared memory segment */


main()
{
	volatile char *buf;	/* Pointer to shared memory. Must be volatile */
				/* to prevent optimizations which would keep */
				/* dereferenced values in registers instead */
				/* of loading from shared memory each time */

				/* first allocate a shared memory segment */
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
				/* move stuff into shared buffer */
	while(1){
		while(*buf == '\0')
			strcpy(buf,stuff);
	}
}
