#include "dmedia/midi.h"
#include <getopt.h>
#include <stdio.h>
#include <sys/syssgi.h>

/* send out a scale */


char *usage = "scale [-o octaves] [-m mode] [-t time] [-s start] [-u] [-d]\n\
    -o sets the number of octaves sounded\n\
    -m sets the diatonic mode, eg, 1 == ionian (major)\n\
       6 == aeolian (minor)\n\
       8 is whole tone\n\
    -b sets the length in milliseconds or ticks of a beat\n\
    -t sets tick mode\n\
    -s sets the start note\n\
    -u and -d play the scale up and down respectively.\n\
    -c sets the MIDI channel used\n\
    -v sets velocity\n\
    -i infinite repetitions\n\
    -f seconds in the future scale will start\n";

char *optstr = "o:m:udtc:s:v:if:b:i";
int uintervals[] = { 2, 2, 1, 2, 2, 2, 1 };
int dintervals[] = { 1, 2, 2, 1, 2, 2, 2 };

playnote(MDport port, char note, unsigned long long time, char channel, char velocity)
{
	MDevent mdv;
	
	mdv.msg[0] = MD_NOTEON | (channel & 0xf);
	mdv.msg[1] = note;
	mdv.msg[2] = velocity;
	
	mdv.stamp = 0;
	mdv.msglen = 3;

	if (mdSend(port, &mdv, 1) < 0) {
		exit(-1);
	}
	
	mdv.stamp = time;
	mdv.msg[2] = 0;
	
	if (mdSend(port, &mdv, 1) < 0) {
		exit(-1);
	}
}

main(int argc, char **argv)
{
	char i, x;
	MDport *mdo;		/* open as many inports as there are */
	int octaves = 8;
	int mode = 0;
	char startnote = 0x48;
	char topnote;
	unsigned long long time = 500 * NSEC_PER_MSEC;
	char upflag = 1, downflag = 0;
	char lazyeight = 0;
	char channel = 0;
	char velocity = 0x48;
	int inp;
	unsigned long long future;
	unsigned long long now;
	int c;

	i = mdInit();
	if (i < 0) {
		printf("MIDI not initialized!\n");
		exit(-1);
	}

	printf("%d devices available\n", i);
	mdo = (MDport *) calloc(i, sizeof(MDport));

	if (!mdo) {
		printf("couldn't allocate array of ports\n");
		exit(-1);
	}

	for (x = 0; x < i; x++) {
		printf("named %s\n", mdGetName(x));
		mdo[x] = mdOpenOutPort(mdGetName(x));
		if (mdo[x] == NULL) 
			printf("open failed\n");
		mdSetStampMode(mdo[x],MD_DELTASTAMP);
	}

	while ((c = getopt(argc, argv, optstr)) > 0) {
		switch (c) {
		case 'o':
			octaves = 8 * atoi(optarg);
			if (!octaves) {
				fprintf(stderr,"illegal value for -o\n");
				octaves = 8;
			}
			break;
		case 'm':
			mode = atoi(optarg) - 1;
			if (mode < 0 || mode > 8) {
				fprintf(stderr,"illegal mode\n");
				mode = 0;
			}
			break;
		case 'u':
			upflag = 1;
			break;
		case 'd':
			downflag = 1;
			break;
		case 'b':
			time = atoi(optarg) * NSEC_PER_MSEC;
			if (!time) {
				fprintf(stderr, "illegal note duration\n");
				time = 500 * NSEC_PER_MSEC;
			}
			break;
		case 'c':
			channel = atoi(optarg) - 1;
			if (channel < 0) {
				fprintf(stderr, "illegal MIDI channel\n");
				channel = 0;
			}
			break;
		case 's':
			startnote = atoi(optarg);
			if (startnote < 1 || startnote > 127) {
				fprintf(stderr, "illegal starting note\n");
				startnote = 0x48;
			}
			break;
		case 'v':
			velocity = atoi(optarg);
			if (velocity <= 0) {
				fprintf(stderr, "illegal velocity\n");
				velocity = 0x48;
			}
			break;
		case 'i':
			lazyeight = 1;
			break;
		case 't':
			mdSetStampMode(mdo[0], MD_DELTATICKS);
			mdSetDivision(mdo[0], 500);
			mdSetTempo(mdo[0], 250000);
			break;
		case 'f':
			inp = atoi(optarg);
			future = inp;
			syssgi(SGI_GET_UST,&now,0);
			future = future * (unsigned long long) 1000000000;
			printf("future %ulld\n", future);
			now = now + future;
			mdSetOrigin(mdo[0],now);
			if (now > future ) printf("should work %ulld\n", future - now);
			else printf("BAD %ulld\n", future - now);
			break;
		case '?':
			printf("usage: %s", usage);
			exit(0);
			break;
		}
	}

	printf("mode %d\n", mode);
	topnote = startnote;
	do {
	if (upflag) {
		for (i = 0; i < octaves; i++ ) {
			printf("sending note\n"); 
			playnote(mdo[0], topnote, time, channel, velocity);
			topnote = uintervals[(i + mode) % 7] + topnote;

		}
	}

	if (downflag) {
		printf("down\n");
		for (i = octaves; i > 0; i--) {
			printf("sending note\n"); 
			topnote -= dintervals[(i + mode) % 7];
			playnote(mdo[0], topnote, time, channel, velocity);

		}
	}
	} while (lazyeight); 
}
