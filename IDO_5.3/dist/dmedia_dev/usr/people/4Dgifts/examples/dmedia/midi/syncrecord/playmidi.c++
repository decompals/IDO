#include <dmedia/midi.h>
#include "midifile.h"
#include "miditrack.h"

#include <audio.h>
#include <audiofile.h>

#include <stdio.h>
#include <malloc.h>
#include <getopt.h>
#include <libc.h>
#include <errno.h>
#include <stropts.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/wait.h>

#define NEVENTS 100

char *optstring = "t:iqv";
int tempo=0, quiet, verbose;

#define CHUNKSIZE 20000
#define NCHANS 2


ALport port;
struct pollfd apfds[1];
AFfilehandle afile;
short abuf[CHUNKSIZE*NCHANS];
    
void
play_audio(void *x)
{
    int afdone = 0;
    int rsize;
    while (!afdone) {
	ALsetfillpoint(port, CHUNKSIZE*NCHANS);
	if (poll(apfds, 1, -1) < 0) {
	    perror("problem in poll");
	    return;
	}
	
	if (!afdone && apfds[0].revents & POLLOUT) {
	    if ((rsize = AFreadframes(afile, AF_DEFAULT_TRACK, abuf, CHUNKSIZE)) == CHUNKSIZE) {
		ALwritesamps(port, abuf, CHUNKSIZE*NCHANS);
	    }
	    else {
		ALwritesamps(port, abuf, rsize*NCHANS);
		afdone=1;
	    }
	}
    }
    while(ALgetfilled(port) > 0) sginap(10);
    ALcloseport(port);
}

main(int argc, char **argv)
{
    MFfile file;
    char mfname[40], afname[40];
    MDevent msgbuf[NEVENTS];
    MDport mdo;
    int mfdone = 0;
    int n = 1, i;
    char *myname;
    long pbuf[2];
    double srate;
    long long ust, msc, nust, nmsc;
    struct pollfd pfds[1];
    char doinit = 0;
    int status;

    myname = argv[0];
    while ((i = getopt(argc,argv,optstring)) != -1)
	switch(i) {
	  case 't' :
	    tempo = atoi(optarg);
	    break;
          case 'i':
	    doinit = 1;
	    break;
          case 'q':
	    quiet = 1;
	    break;
          case 'v':
	    verbose = 1 ;
	    break;
	}

    i = mdInit();
    if (i < 0) {
	    printf("MIDI not initialized!\n");
	    exit(-1);
    }

    mdo = mdOpenOutPort(0);
    if (mdo == NULL) 
	    printf("open failed\n");
    mdSetStampMode(mdo,MD_RELATIVETICKS);
    
    pfds[0].fd = mdGetFd(mdo);
    pfds[0].events = POLLOUT;

    if (optind == argc) {
	printf("please supply a midi file\n");
	exit(0);
    }

    sprintf(mfname, "%s.mid", argv[optind]);
    sprintf(afname, "%s.aifc", argv[optind]);
    if ((afile = AFopenfile(afname, "r", 0)) == 0) {
	fprintf(stderr, "could not open file %s\n", afname);
	exit(-1);
    }
    srate = AFgetrate(afile, AF_DEFAULT_TRACK);
    pbuf[0] = AL_OUTPUT_RATE;
    pbuf[1] = srate;
    ALsetparams(AL_DEFAULT_DEVICE, pbuf, 2);
    srate = 1000000000.0 / srate;	    /* now in nanosecs per sample */

    file.open(mfname,"r");
    file.read();
    if (tempo) 
	    file.settempo(tempo);
    else
	    mdSetTempo(mdo, file.gettempo());

    file.rewind();
    
    port = ALopenport(myname, "w", 0);
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
    apfds[0].events = POLLOUT;
    
    /*
     * bring the audio port out of underflow. Otherwise,  we cannot
     * calculate the MSC & UST relationships.
     */
    bzero(abuf, CHUNKSIZE*NCHANS*sizeof(short));
    ALwritesamps(port, abuf,CHUNKSIZE*NCHANS);
    
    ALgetframenumber(port, &msc);
    ALgetframetime(port, &nmsc, &nust);
    ust = nust - (nmsc-msc)*srate;
    
    mdSetOrigin(mdo,ust);
    mdSetDivision(mdo,file.division()); 

    printf("playing file\n");
    sproc(play_audio, PR_SALL);
    while (!mfdone) {
	if (poll(pfds, 1, -1) < 0) {
	    perror("problem in poll");
	    return -1;
	}
	
	if (!mfdone && pfds[0].revents & POLLOUT) {
	    if (n = file.nextevent(msgbuf,NEVENTS)) {
		if (!quiet)
			mdSend(mdo,msgbuf,n);
		if (verbose) {
			char prbuf[800];
			mdPrintEvent(prbuf,msgbuf,n);
			printf("%s", prbuf);
		}
	    }
	    else {
		mfdone = 1;
	    }
	}
    }
    wait(&status);
    printf("done\n");
    return 0;
}
