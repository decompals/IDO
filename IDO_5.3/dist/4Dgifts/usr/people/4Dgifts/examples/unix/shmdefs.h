/*
 *   shmdefs.h:
 *
 *    Used by the shmem1 and shmem2 shared memory demo program set.
 *
 *  References:  INTRO(2), SHCTL(2), SHGET(2), SHMOP(2), SPROC(2)
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>

#define default_key 0xffff0000
#define default_size 2048
#define default_mode 0777

extern key_t key;
extern int size;
extern int mode;
