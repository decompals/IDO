#include <gl/gl.h>

main()
{
    int i, j;

    prefsize(400, 400);
    winopen("pointsquare");
    color(BLACK);
    clear();
    color(GREEN);
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 100; j++)
	    pnt2i(i*4 + 1, j*4 + 1);
    }
    sleep(10);
    gexit();
    return 0;
}
