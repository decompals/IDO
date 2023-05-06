#include <dmedia/midi.h>
#include "sys/select.h"

/* test MIDI port reading */

main()
{
	int nports, x;
	MDport inport, outport;
	MDevent mdv;
	fd_set inports, outports;
	int nfds, highfd;

	nports = mdInit();
	printf("%d devices available\n", nports);

	inport = mdOpenInPort(0);
	if (inport == NULL) 
		printf("open failed\n");

	outport = mdOpenOutPort(0);
	if (outport == NULL) 
		printf("open failed\n");

	mdSetStampMode(inport, MD_NOSTAMP);
	mdSetStampMode(outport, MD_NOSTAMP);

	FD_SET(mdGetFd(inport),&inports);
	FD_SET(mdGetFd(outport),&outports);
	highfd = mdGetFd(outport) + 1; 

	while(1) {
		nfds = select(highfd,&inports,0,0,0);
		for (x = 0; x < nports; x++) {
			if (FD_ISSET(mdGetFd(inport),&inports)) {
				if (mdReceive(inport, &mdv, 1) < 0) {
					printf("failure receiving message\n");
					abort();
					exit(-1);
				}
				if (mdSend(outport, &mdv, 1) < 0) {
					printf ("failure sending message\n");
					exit(-1);
				}
				if (mdv.msg[0] == MD_SYSEX) mdFree(mdv.sysexmsg);
			} else {
				FD_SET(mdGetFd(inport),&inports);
			}
		}
	}
}
