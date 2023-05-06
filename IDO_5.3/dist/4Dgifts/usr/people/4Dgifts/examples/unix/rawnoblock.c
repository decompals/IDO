/*
 *   rawnoblock.c:
 *    
 *    Demostrates how to read a tty device using "raw" (character-at-a-
 *  time) non-blocking reads (see the setraw() routine for all termio 
 *  flags which are set for raw i/o reads).
 *
 *  References:  TERMIO(7), OPEN(2), ALARM(2), SIGNAL(2), IOCTL(2),
 *               GETOPT(3C), READ(2), WRITE(2) 
 *
 *                                     Grant Dorman - 1990
 */

#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <termio.h>
#include <string.h>
#include <sys/signal.h>
#include <sys/types.h>

#define   RUNTIME   20  /* 20 seconds max running time. */

#define   keybd "/dev/tty"

char   *usage = "usage: -dport#";
char   ch, rs232[10];
int    f1, f2;
struct termio old_f1, old_f2, new_f1, new_f2;

extern int   optind;
extern char *optarg;


main (argc, argv) /* rawnoblock: raw nonblocking read of keybd/rs232 port. */
int    argc;
char **argv;
{
    register int c;
    register char *pname;

    pname = *(argv+0);
    if (argc == 1)  { 
        fprintf(stderr, "%s: %s\n", pname, usage); 
        exit(2); 
    }
    while ((c = getopt(argc, argv, "d:")) != EOF) {
        switch (c) {
            case 'd': 
                sprintf(rs232, "/dev/ttyd%s", optarg); 
                break;
            case '?': 
                fprintf(stderr, "%s: %s\n", pname, usage); 
                exit(2); 
        }
    }
    initialize();
    for (;;)
        if (read(f1, &ch, 1) != NULL)
            printf("console: %c\n", ch);
        else if (read(f2, &ch, 1) != NULL)
            printf("rs232: %c\n", ch);
        else 
	    write(1,".",1);
}



initialize ()
{
    extern int cleanup();
    extern struct termio old_f1, old_f2, new_f1, new_f2;

    if ((f1 = open(keybd, O_RDONLY|O_NDELAY, 0666)) == EOF) { 
	perror("open"); 
	exit(1); 
    }
    if ((f2 = open(rs232, O_RDONLY|O_NDELAY, 0666)) == EOF) { 
	perror("open"); 
	exit(2); 
    }
    setraw(f1,&old_f1, &new_f1);
    setraw(f2, &old_f2, &new_f2);
    alarm(RUNTIME);
    signal(SIGINT, cleanup);  /* stop program if a kill -2 is issued. */
    signal(SIGALRM, cleanup); /* after RUNTIME seconds, stop program. */
    return;
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



cleanup ()  /* someone has sent an interrupt signal, time to clean up. */
{ 
    extern struct termio old_f1, old_f2, new_f1, new_f2;

    if (ioctl(f1, TCSETAF, &old_f1) == -1) { 
	perror("ioctl"); 
	exit(5); 
    }
    if (ioctl(f2, TCSETAF, &old_f2) == -1) { 
	perror("ioctl"); 
	exit(6); 
    }
    exit(0);
}
