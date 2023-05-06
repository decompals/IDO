#include <stdio.h>
#include <stdlib.h>
#include <gl/gl.h>

main(argc, argv)
int argc;
char *argv[];
{
    int i, nsteps;
    short val;
    short ramp[256];

    if (argc != 2) {
	fprintf(stderr, "Usage: stepgamma <numsteps>\n");
	return 1;
    }
    nsteps = atoi(argv[1]);
    if (nsteps < 2) {
	fprintf(stderr, "stepgamma: <numsteps> cannot be < 2\n");
	return 1;
    }

    noport();
    winopen("stepgamma");
    for (i = 0; i < 256; i++) {
	val = ((nsteps * i) / 256) * 255 / (nsteps - 1);
	if (val > 255)
	    val = 255;
	ramp[i] = val;
    }
    gammaramp(ramp, ramp, ramp);
    gexit();
    return 0;
}
