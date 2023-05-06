/*
 * record.c - record digital audio data to disk until interrupt signal
 */
 
#include <dmedia/audio.h>     /* audio library header */
#include <sys/types.h> /* for unix open() */
#include <sys/stat.h>  /* for unix open() */
#include <fcntl.h>     /* for unix open() */
#include <signal.h>    /* for signal handler */

int caught_sigint =  0;     /* completion flag */ 

void 
catch_sigint()
{
   caught_sigint=1;
}

main()
{
    /*
     * record 44.1 KHz 16-bit stereo data
     * to a (raw data) file until user sends an interrupt
     */
    short buf[44100 * 2];         /* 1 sec of 44.1 KHz stereo      */
    long nsamps = 44100 * 2;      /*    16-bit data                */
    int fd;                       /* output file                   */
    ALconfig config;              /* port configuration structure  */
    ALport inport;                /* audio port structure          */

    fd = open("recordfile", O_CREAT| O_WRONLY | O_TRUNC);/* open disk file           */
    config = ALnewconfig();             /* create a config object   */
    ALsetqueuesize(config, 44100*2*2);  /* 2 sec of stereo          */
    ALsetwidth(config, AL_SAMPLE_16);  	/* 16-bit signed samples    */
    ALsetchannels(config, AL_STEREO);  	/* stereo port              */
    sigset(SIGINT, catch_sigint);       /* set signal handler       */

                                        
    inport = ALopenport("inport", "r", config); 

    while (!caught_sigint)
    {  
        ALreadsamps(inport, buf, nsamps);     /* read 1 sec of sound */
        write(fd, buf, nsamps*sizeof(short)); /* record to disk      */
    }

    ALfreeconfig(config);               /* free the config object    */
    ALcloseport(inport);                /* shut down the input port  */
    close(fd);                          /* close the output file     */
}
