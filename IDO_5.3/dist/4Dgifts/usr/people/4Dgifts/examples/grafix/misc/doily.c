/*
 * doily.c:
 *
 *   draws a doily depending on how many points you give it (range is 
 *   currently set between 3..100).  point count is equivalent to how
 *   many line segments make up the circle's edge.
 */
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

#define PI 3.1415926535
float points[100][2];

main(argc, argv)
int argc;
char *argv[];
{
    short val;
    Device dev;
    long numpts;

    /* First figure out how many points there are. */
    if (argc != 2) {
        printf("Usage: %s <point_count (range: [3..100])>\n", argv[0]);
        exit(0);
    }
    numpts = atoi(argv[1]);        /* convert argument to internal format */

    if (numpts > 100) {
        printf("Too many points\n");
        exit(0);
    } else if (numpts < 3) {
        printf("Too few points\n");
        exit(0);
    }
    initialize(numpts);

    while (TRUE) {
        if (qtest()) {
            dev = qread(&val);
            switch (dev) {
                case ESCKEY:
                    gexit();
                    exit(0);
                    break;
                case REDRAW:
                    reshapeviewport();
                    drawdoily(numpts);
                    break;
            }
        }
    }       
}

initialize(numpts)
long numpts;
{
    int gid;
    long i;

    /* Now get the x and y coordinates of numpts equally-
     * spaced points around the unit circle.
     */
    for (i = 0; i < numpts; i++) {
        points[i][0] = cos((i*2.0*PI)/numpts);
        points[i][1] = sin((i*2.0*PI)/numpts);
    }

    keepaspect(1,1);
    gid = winopen("doily");

    qdevice(ESCKEY);
    qenter(REDRAW,gid);

    ortho2(-1.2, 1.2, -1.2, 1.2);
}

drawdoily(numpts)
long numpts;
{
    long i,j;

    color(BLACK);
    clear();
    color(RED);

    for (i = 0; i < numpts; i++)
        for (j = i+1; j < numpts; j++) {
            move2(points[i][0], points[i][1]);
            draw2(points[j][0], points[j][1]);
        }
}
