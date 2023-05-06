/*
 * Simple Output Window
 * Change output location of NTSC video window
 */

#include <stdio.h>
#include <stdlib.h>
#include <svideo.h>

main(int argc, char *argv[])
{
    int xstart, ystart;
    SVhandle V;

    if (argc != 3) {
	fprintf(stderr, "Usage: %s x_start y_start\n", argv[0]);
	exit(1);
    }
    xstart = atoi(argv[1]);
    ystart = atoi(argv[2]);

    /* open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }
    /* change location of output window */
    svOutputOffset(V, xstart, ystart);
    exit(0);
}
