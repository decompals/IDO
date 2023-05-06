/* 
 * Synchronized MIDI + audio file recorder.
 * 
 * usage: recordmidi [filename]
 * 
 * default filename is "test.mid".  recordmidi will overwrite
 * the input file.
 *
 * Known bug: the tempo is written twice.
 * 
 */

#include <limits.h>
#include <dmedia/midi.h>
#include <midifile.h>
#include <miditrack.h>
#include <stdio.h>
#include <malloc.h>
#include <stropts.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <audio.h>
#include <audiofile.h>
#include <sys/prctl.h>
#include <sys/schedctl.h>
#include <sys/wait.h>

#define NCHANS 2
#define NSEC_PER_SEC_D (1000000000.0)

int
get_input_rate()
{
    long buf[6];

    buf[0] = AL_INPUT_RATE;
    buf[2] = AL_INPUT_SOURCE;
    buf[4] = AL_DIGITAL_INPUT_RATE;
    ALgetparams(AL_DEFAULT_DEVICE,buf,6);

    if (buf[1] == AL_RATE_AES_1 || buf[3] == AL_INPUT_DIGITAL) {
        /*
         * we are clocked off of the digital input. find the
         * real input rate, if we can.
         */
        if (ALgetdefault(AL_DEFAULT_DEVICE,AL_DIGITAL_INPUT_RATE) >= 0) {
            return buf[5];
        }
    }
    else if (buf[1] > 0) {
        /*
         * input rate is in Hz and we're using an analog input -- return rate.
         */
        return buf[1];
    }
    return AL_RATE_UNDEFINED;
}

char prbuf[800];
struct pollfd pfds[3];		// check the midi & stdin fd's for termination
struct pollfd apfds[3];		// check the midi & stdin fd's for termination
struct stat statbuf;
int done = 0;
#define FNAME "test"


char         *myname;             /* name of this program              */
ALconfig      portconfig;         /* audio port configuration          */
ALport        port;               /* audio port                        */
long          portrate;           /* audio port sampling rate          */
AFfilesetup   filesetup;          /* audio file setup                  */
AFfilehandle  afile;              /* audio file handle                 */
double        filerate;           /* audio file sampling rate          */
void         *buf;                /* sample transfer buffer            */
int           numframeswrit;      /* number of frames written          */
int           samplesperbuf;      /* samples transfered per loop       */
int           framesperbuf;       /* sample frames transfered per loop */
int           samplespersec;      /* samples transfered per sec        */

void
audio_loop(void *x)
{
    do {
	ALsetfillpoint(port, samplesperbuf);
	if (poll(apfds, 1, -1) < 0) {
	    perror("problem in poll");
	    _exit(-1);
	}
	
	if (apfds[0].revents & POLLIN) {
	    ALreadsamps(port, buf, samplesperbuf);
	    if ((numframeswrit = AFwriteframes(afile, AF_DEFAULT_TRACK, 
		buf, framesperbuf)) < framesperbuf) {
	    }
	    
	}

    } while (!done);
    AFclosefile(afile);   /* this is important: it updates the file header */
    ALcloseport(port);
}

main(int argc, char **argv)
{
    MFfile file;
    MFtrack *track;
    MDevent e;
    char msg[9];
    int tempo = 500000;
    MDport ip, op;
    int nev = 0;
    char *fname = FNAME;
    char mfname[40], afname[40];
    int	status;
    double srate;
    unsigned long long ust, msc, nust, nmsc;

    myname = argv[0];
    if (argc > 1) fname = argv[1];

    sprintf(mfname, "%s.mid", fname);
    sprintf(afname, "%s.aifc", fname);
    
    if (stat(mfname, &statbuf) < 0) {
	    if (errno != ENOENT) {
		perror("problem with midi file");
		return -1;
	    }
    } else {
	    int ans;
	    printf("clobber file %s? (y/n)", mfname);
	    ans = getchar();
	    if (ans == 'y') {
		unlink(mfname);
		unlink(afname);
	    }
	    else return 0;
    }
    
    /*
     * get the current input rate in use by the hardware.
     */
    if ((portrate = get_input_rate()) == AL_RATE_UNDEFINED) {
	fprintf(stderr, "Can't get the current input rate\n");
	exit(-1);
    }
    filerate = portrate;
    srate = (NSEC_PER_SEC_D/filerate);
    
    /*
     * compute the number of input samples equal to half a 
     * second and allocate a transfer buffer
     */
    samplespersec   = (portrate) * NCHANS;
    samplesperbuf   = samplespersec / 2;    /* half second buffer */
    framesperbuf    = samplesperbuf / NCHANS;    
    buf             = (short *)malloc(samplesperbuf * sizeof(short));
    
    
    /* 
     * configure an audio file 
     */
    filesetup    = AFnewfilesetup();
    AFinitfilefmt(filesetup, AF_FILE_AIFFC); 
    AFinitchannels(filesetup, AF_DEFAULT_TRACK,  NCHANS);
    AFinitrate(filesetup, AF_DEFAULT_TRACK, filerate);
    AFinitsampfmt(filesetup, AF_DEFAULT_TRACK, 
                      AF_SAMPFMT_TWOSCOMP, 16); /* in bits */
		      
    /*
     * open the audio file
     */
    afile = AFopenfile(afname, "w", filesetup);
    if (!afile) {
	fprintf(stderr, "Could not open file %s\n", afname);
	exit(-1);
    }
    
    file.open(mfname,"w");

    file.setformat(1);
    file.setdivision(480);
    file.setppq(24);
    file.settimesig(4,4);
    file.setthirt2s(8);
    file.settempo(tempo);
    
    file.newtrack();

    track = file.gettrack(0);

    msg[0] = 0xff;
    msg[1] = 0x51;
    msg[2] = 3;
    msg[3] = (tempo & 0xff0000) >> 16;
    msg[4] = (tempo & 0xff00) >> 8;
    msg[5] = (tempo & 0xff);

    e.sysexmsg = msg;
    e.msglen = 6;
    e.stamp = 0;

    track->insertevent(&e);
    file.newtrack();
    track = file.gettrack(1);

    if (mdInit() <= 0) return 0;

    if ((ip = mdOpenInPort(0)) == 0) {
	    perror("couldn't open inport");
	    return -1;
    }

    pfds[0].fd = 0;
    pfds[0].events = POLLIN;
    pfds[0].revents = 0;

    pfds[1].fd = mdGetFd(ip);
    pfds[1].events = POLLIN;
    pfds[1].revents = 0;

    if ((op = mdOpenOutPort(0)) == 0) {
	    perror("couldn't open outport");
	    return -1;
    } 
    
    /*
     * open the audio port
     */
    portconfig    = ALnewconfig();
    ALsetchannels(portconfig, NCHANS);
    ALsetwidth(portconfig, AL_SAMPLE_16);
    ALseterrorhandler(0);		    /* shut up default AL error handler */
    
    port = ALopenport(myname, "r", portconfig);
    if (!port) {
	int err = oserror();
	if (err == AL_BAD_NO_PORTS) {
	    fprintf(stderr, " System is out of audio ports\n");
	} else if (err == AL_BAD_DEVICE_ACCESS) {
	    fprintf(stderr, " Couldn't access audio device\n");
	} else if (err == AL_BAD_OUT_OF_MEM) {
	    fprintf(stderr, " Out of memory: port open failed\n");
	}
	exit(1);
    }
    apfds[0].fd = ALgetfd(port);
    apfds[0].events = POLLIN;

    mdSetStampMode(ip, MD_DELTATICKS);
    mdSetTempo(ip, tempo);
    mdSetDivision(ip, 480);

    mdSetStampMode(op, MD_NOSTAMP);
    
    /*
     * Calculate the UST associated with the next audio sample frame to be read
     * from the input port.
     * 
     * 1. Get the frame number of the frame. Call this "msc."
     * 2. Get a pair of (UST, frame number) at the jacks.
     * 3. Use this pair to calculate the UST for when the frame "msc" was at the jacks.
     */
    ALgetframenumber(port, &msc);
    ALgetframetime(port, &nmsc, &nust);
    ust = nust - (nmsc-msc)*srate;
    
    /*
     * make the MIDI events relative to when our first audio sample
     * hit the jacks.
     */
    mdSetOrigin(ip, ust);

    sproc(audio_loop, PR_SALL);
    printf("audio UST: %lld\n", ust);
    printf("recording... hit <enter> to stop\n");
    schedctl(NDPRI, 36);
    do {
	if (poll(pfds, 3, -1) < 0) {
		perror("problem in poll");
		return -1;
	}
	
	if (pfds[1].revents & POLLIN) {
		if (mdReceive(ip, &e, 1) < 0) {
			perror("mdReceive failure");
			return -1;
		}

		if (mdSend(op, &e, 1) < 0) {
			perror("mdSend failure");
			return -1;
		}

		track->insertevent(&e);
		mdPrintEvent(prbuf,&e,1);
		printf("%s", prbuf);
		nev++;
	} 

	if (pfds[0].revents & POLLIN) {
		done = 1;
	}
	    
    } while (!done);

    wait(&status);
    printf("inserted %d\n", nev);
    track->rewind();
    file.writefile();
    return 0;
}
