#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#define KEY   99

struct smem { 
    float x, y, xy; 
    char ch[25]; 
};

struct smem *shmget_ ()
{
    char *saddr;
    register int id;

    if ((id = shmget((key_t) KEY, sizeof (struct smem), 0664|IPC_CREAT)) < 0) {
	perror("shmget"); 
	exit(-1); 
    }
    if ((saddr = (char *) shmat(id, 0, 0)) < (char *) 0) { 
	perror("shmat"); 
	exit(-1); 
    }
    return((struct smem *) saddr);
}
