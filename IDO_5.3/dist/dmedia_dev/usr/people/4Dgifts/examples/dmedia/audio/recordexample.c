#include <stdio.h>
#include <signal.h>
#include <dmedia/audio.h>
#include <dmedia/audiofile.h>

/*
 * small example program: "recordexample"
 *
 * record an AIFF-C file from an audio input port
 * stop recording when user sends an interrupt
 *
 * file is configured for 16-bit stereo data at the current
 *     sampling rate of the audio hardware
 *
 * usage: "recordexample <filename>"
 */
int      caught_sigint;

/*
 * catch interrupt signal
 */
static void
catch_sigint()
{
    caught_sigint++;
}

main(int argc, char **argv)
{
    char         *myname;             /* name of this program              */
    char         *portname;           /* audio port name                   */
    ALconfig      portconfig;         /* audio port configuration          */
    ALport        port;               /* audio port                        */
    long          portchannels;       /* audio port channels               */
    long          portrate;           /* audio port sampling rate          */
    long          portsampwidth;      /* audio port sample width           */
    long          portsampfmt;        /* audio port sample format          */
    AFfilesetup   filesetup;          /* audio file setup                  */
    AFfilehandle  file;               /* audio file handle                 */
    char         *filename;           /* audio file name                   */
    long          filechannels;       /* audio file channels               */
    double        filerate;           /* audio file sampling rate          */
    long          filesampwidth;      /* audio file sample width           */
    long          filesampfmt;        /* audio file sample format          */
    long          pvbuf[2];           /* parameter-value buffer            */
    void         *buf;                /* sample transfer buffer            */
    int           numframeswrit;      /* number of frames written          */
    int           done;               /* flag                              */
    int           samplesperbuf;      /* samples transfered per loop       */
    int           framesperbuf;       /* sample frames transfered per loop */
    int           samplespersec;      /* samples transfered per sec        */

    myname   = argv[0];
    portname = myname;

    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s filename\n", myname);
        exit(1);
    }

    sigset(SIGINT, catch_sigint);

    filename = argv[1];
    /*
     * get the global IRIS Audio Processor input rate
     */
    pvbuf[0] = AL_INPUT_RATE;
    ALgetparams(AL_DEFAULT_DEVICE, pvbuf, 2);
    portrate = pvbuf[1];

    /*
     * initialize the audio port and audio file configuration
     */
    portchannels    = AL_STEREO;              /* port channels      */
    portsampwidth   = AL_SAMPLE_16;           /* port sample width  */
    portsampfmt     = AL_SAMPFMT_TWOSCOMP;    /* port sample format */
    filechannels    = 2;                      /* file  channels     */
    filesampwidth   = 16;                     /* file sample width  */
    filesampfmt     = AF_SAMPFMT_TWOSCOMP;    /* file sample format */
    /*
     * configure file sample rate to match IRIS audio processor input rate
     */
    switch (portrate)
    {
       case AL_RATE_48000: filerate = 48000.0; break;
       case AL_RATE_44100: filerate = 44100.0; break;
       case AL_RATE_32000: filerate = 32000.0; break;
       case AL_RATE_22050: filerate = 22050.0; break;
       case AL_RATE_16000: filerate = 16000.0; break;
       case AL_RATE_11025: filerate = 11025.0; break;
       default:
       case AL_RATE_8000: filerate =  8000.0; break;
    }
    /*
     * compute the number of input samples equal to half a 
     * second and allocate a transfer buffer
     */
    samplespersec   = ((long)filerate) * 2; /* stereo             */
    samplesperbuf   = samplespersec / 2;    /* half second buffer */
    framesperbuf    = samplesperbuf / 2;    /* stereo             */
    buf             = (short *)malloc(samplesperbuf * sizeof(short));
    /*
     * open the audio port
     */
    portconfig    = ALnewconfig();
    ALsetchannels(portconfig, portchannels);
    ALsetwidth(portconfig, portsampwidth);
    ALsetqueuesize(portconfig, samplesperbuf);
    port = ALopenport(portname, "r", portconfig);
    /* 
     * configure an audio file 
     */
    filesetup    = AFnewfilesetup();
    AFinitfilefmt(filesetup, AF_FILE_AIFFC); 

    AFinitchannels(filesetup, AF_DEFAULT_TRACK,  filechannels);
    AFinitrate(filesetup, AF_DEFAULT_TRACK, filerate);
    AFinitsampfmt(filesetup, AF_DEFAULT_TRACK, 
                      AF_SAMPFMT_TWOSCOMP, filesampwidth); /*in bits */
    /*
     * open the audio file
     */
    file = AFopenfile(filename, "w", filesetup);
    /*
     * play the buffer
     */
    done = 0;
    caught_sigint = 0;
    while (!done && !caught_sigint)
    {
        ALreadsamps(port, buf, samplesperbuf);
        if ((numframeswrit 
                 = AFwriteframes(file, AF_DEFAULT_TRACK, 
                             buf, framesperbuf)) < framesperbuf)
        {
            done++;
        }
    }

    AFclosefile(file);   /* this is important: it updates the file header */
    ALcloseport(port);
    exit(0);
}
