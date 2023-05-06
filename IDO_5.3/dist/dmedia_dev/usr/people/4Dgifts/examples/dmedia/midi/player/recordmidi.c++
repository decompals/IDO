/* really primitive MIDI file recorder.  records events until 
 * <enter> is hit on the keyboard
 * 
 * usage: recordmidi [filename]
 * 
 * default filename is "test.mid".  recordmidi will overwrite
 * the input file.
 *
 * Known bug: the tempo is written twice.
 * 
 */
 


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


char prbuf[800];
struct pollfd pfds[2];		// check the midi & stdin fd's for termination
struct stat statbuf;
#define FNAME "test.mid"

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
    int done = 0;

    if (argc > 1) fname = argv[1];

    if (stat(fname, &statbuf) < 0) {
	    if (errno != ENOENT) {
		    perror("problem with file");
		    return -1;
	    }
    } else {
	    int ans;
	    printf("clobber file %s? (y/n)", fname);
	    ans = getchar();
	    if (ans == 'y') unlink(fname);
	    else return 0;
    }

    file.open(fname,"w");

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

    printf("recording... hit <enter> to stop\n");

    mdSetStampMode(ip, MD_DELTATICKS);
    mdSetTempo(ip, tempo);
    mdSetDivision(ip, 480);

    mdSetStampMode(op, MD_NOSTAMP);

    do {
	    if (poll(pfds, 2, -1) < 0) {
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

    printf("inserted %d\n", nev);
    track->rewind();
    file.writefile();
    return 0;
}
