/*
 *   rawselect.c
 *
 *    Demonstrates how to read a tty device using "raw" (one-character-at-
 *  a-time) canonical input-disabled reads.   See the setraw() routine for
 *  all the termio flags which are set for raw i/o reads.  This program 
 *  also shows how to set and use the SELECT(2) system call (useful so one
 *  can select among several possible tty ports).
 *    All the FD_xxx macros hail from the SELECT(2) system call.
 *
 *  References:  TERMIO(7), SELECT(2), SIGNAL(2), ALARM(2), IOCTL(2),
 *               READ(2), WRITE(2)
 *
 *                                     Grant Dorman - 1990
 */

#include <stdio.h>
#include <fcntl.h>
#include <termio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/signal.h>

#define   RUNTIME   20   /* 20 seconds max running time. */

int    f1 = 0;           /* using stdin;  i.e. keyboard in this example. */
char   *msg = "time out...\r\n";
fd_set fdset;
struct timeval timeout;
struct termio old_f1, new_f1;


main ()  /* rawselect: raw read of keyboard using select system call. */
{
    extern int f1;
    int nread, nfound;
    char buf[BUFSIZ];

    initialize();
    for (;;) { 
	FD_SET(f1, &fdset);        
        if ((nfound = select(FD_SETSIZE, &fdset, 0, 0, &timeout)) < 0) { 
	    perror("select"); 
	    exit(1); 
	} else if (FD_ISSET(f1, &fdset)) { 
	    if ((nread = read(f1, buf, BUFSIZ)) > 0)
                write(f1, buf, nread); 
	} else if (nfound == 0) 
	    write(2, msg, 13);

        FD_CLR(f1, &fdset);
    }
}


initialize ()
{
    extern int f1, cleanup();
    extern fd_set fdset;
    extern struct timeval timeout;
    extern struct termio old_f1, new_f1;
 
    setraw(f1,&old_f1, &new_f1);
    FD_ZERO(&fdset);
    timeout.tv_sec = 2; timeout.tv_usec = 0;  /* select timeout values */
    alarm(RUNTIME);
    signal(SIGINT, cleanup);  /* stop program on kill -2. */
    signal(SIGALRM, cleanup); /* after RUNTIME seconds, stop program. */
}


setraw (fd, rsold, rsnew)
int fd;
struct termio *rsold, *rsnew;
{
    if (ioctl(fd, TCGETA, rsold) == -1) { 
	perror("ioctl"); 
	exit(3); 
    }
    *rsnew = *rsold;
    rsnew->c_oflag &= ~OPOST; rsnew->c_cc[VMIN] = 1; rsnew->c_cc[VTIME] = 0;
    rsnew->c_lflag &= ~(ICANON|ISIG|XCASE|ECHO);
    rsnew->c_iflag &= ~(INLCR|ICRNL|IUCLC|ISTRIP|IXON|BRKINT);
    if (ioctl(fd, TCSETAF, rsnew) == -1) { 
	perror("ioctl"); 
	exit(4); 
    }
}


cleanup ()  /* interrupt signal, time to clean up. */
{ 
    extern struct termio old_f1, new_f1;

    if (ioctl(f1, TCSETAF, &old_f1) == -1) { 
	perror("ioctl"); 
	exit(5); 
    }
    exit(0);
}
