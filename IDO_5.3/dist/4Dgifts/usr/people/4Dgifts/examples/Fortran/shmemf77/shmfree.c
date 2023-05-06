#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>


shmfree_ (ptr)
char *ptr;
{ 
    if (shmdt(ptr) < 0) 
	fprintf(stderr, "shmfree: can't detach.\n"); 
}
