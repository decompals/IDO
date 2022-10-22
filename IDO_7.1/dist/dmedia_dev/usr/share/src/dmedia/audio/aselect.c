/*
 * aselect.c - demostrate use of select(2) call and audio library programming
 */

#include <dmedia/audio.h>     /* audio library header */
#include <sys/types.h> /* for open() */
#include <sys/stat.h>  /* for open() */
#include <fcntl.h>     /* for open() */
#include <signal.h>    /* for sigset() */
#include <sys/time.h>  /* timeval for select() */
#include <sys/prctl.h>     /* for schedctl() */
#include <sys/schedctl.h>  /* for schedctl() */
#include <stdio.h>

int caught_sigint;

static void 
catch_sigint()
{
   caught_sigint = 1;
}

main()
{
    /*
     * record 44.1 KHz 16-bit stereo data
     * to a (raw data) file until user sends an interrupt
     */
    short buf[44100*2*2];  /* 2 sec of stereo 16-bit data  */
                           /* at 44.1 KHz                  */
    long nsamps;
    int fd;                /* output file */
    ALconfig config;       /* port configuration structure */
    ALport inport;         /* audio port structure         */
    int    inportfd;       /* port file descriptor         */
    fd_set read_fds;
    int nfds;
    int r;

    int pid;               /* process id */
    int result;

    pid = getpid();

    result = schedctl(NDPRI, pid, NDPHIMIN);    
    if (result == -1) 
    {
        fprintf(stderr, "error setting process priority.\n");
        exit(-1);
    }

    bzero(buf, 44100*2*2*sizeof(short));
    fd = open("outfile", O_RDWR|O_CREAT|O_TRUNC, 0644);
    config = ALnewconfig();
    ALsetqueuesize(config, 44100*2*2);  /* 2 secs of queue space */
    ALsetwidth(config, AL_SAMPLE_16);  	/* 16-bit signed samples */
    ALsetchannels(config, AL_STEREO);  	/* stereo port           */
 
    caught_sigint = 0;
    sigset(SIGINT, catch_sigint);       /* set sig handler       */

    inport   = ALopenport("inport", "r", config); 
    inportfd = ALgetfd(inport);
    FD_ZERO(&read_fds);                /* clear read_fds        */
    while (!caught_sigint)
    {  
        nsamps = ALgetfilled(inport);        /* get number of   */
                                             /* queued samples  */
        ALreadsamps(inport, buf, nsamps);    /* read as many as */
                                             /* possible        */
 
        write(fd, buf, nsamps*sizeof(short));/* record to disk  */

        /* prepare for next call to select*/
        ALsetfillpoint(inport, 44100*2);     /* fillpoint =1 sec*/
        nfds = inportfd + 1;
        FD_SET(inportfd, &read_fds);            
        r = select(nfds, &read_fds, (fd_set *)0, (fd_set *)0, 
                        (struct timeval *)0);
        if (r < 0)  /* select reported an error */
           break;
    }
    ALfreeconfig(config);
    ALcloseport(inport);
    close(fd);
}
