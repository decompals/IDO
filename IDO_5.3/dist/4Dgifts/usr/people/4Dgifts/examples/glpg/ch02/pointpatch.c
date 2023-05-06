#include <gl/gl.h>

main()
{
    long vert[2];
    int i, j;

    prefsize(400, 400);
    winopen("pointpatch");
    color(BLACK);
    clear();
    color(WHITE);
    for (i = 0; i < 20; i++) {
	vert[0] = 100 + 10 * i;		/* load the x coordinate */
	bgnpoint();
	for (j = 0; j < 20; j++) {
	    vert[1] = 100 + 10 * j;	/* load the y coordinate */
	    v2i(vert);			/* draw the point */
	}
	endpoint();
    }
    sleep(10);
    gexit();
    return 0;
}
