#include <dmedia/midi.h>
#include "midifile.h"
#include "miditrack.h"

#include <stdio.h>
#include <malloc.h>
#include <getopt.h>
#include <libc.h>

#define NEVENTS 100

char *optstring = "t:iqv";
int tempo, quiet, verbose;

main(int argc, char **argv)
{
    MFfile file;
    MDevent msgbuf[NEVENTS];
    MDport *mdo;
    int n = 1, i, x;
    char doinit = 0;

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

    mdo = (MDport *) calloc(i, sizeof(MDport));

    if (!mdo) {
	    printf("couldn't allocate array of ports\n");
	    exit(-1);
    }

    for (x = 0; x < i; x++) {
	    mdo[x] = mdOpenOutPort(mdGetName(x));
	    if (mdo[x] == NULL) 
		    printf("open failed\n");
	    mdSetStampMode(mdo[x],MD_RELATIVETICKS);
    }

    if (optind == argc) {
	printf("please supply a midi file\n");
	exit(0);
    }

    file.open(argv[optind],"r");
    file.read();
    if (tempo) 
	    file.settempo(tempo);
    else
	    mdSetTempo(mdo[0], file.gettempo());

    file.rewind();
    mdSetOrigin(mdo[0],0);
    mdSetDivision(mdo[0],file.division()); 

    printf("playing file\n");
    while (n = file.nextevent(msgbuf,NEVENTS)) {
	    if (!quiet)
		    mdSend(mdo[0],msgbuf,n);
	    if (verbose) {
		    char prbuf[800];
		    mdPrintEvent(prbuf,msgbuf,n);
		    printf("%s", prbuf);
	    }
    }
    printf("done\n");
    return 0;
}
