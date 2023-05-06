#include <gl/gl.h>

float p[3][2] = {
    {0.0, 0.0},
    {0.6, 0.0},
    {0.0, 0.6}
};

main()
{
    int i;

    prefsize(400, 400);
    winopen("rasterchars2");
    ortho2(-1.0, 1.0, -1.0, 1.0);
    for (i = 0; i < 40; i++) {
	color(BLACK);
	clear();
	rotate(50, 'z');
	color(RED);
	bgnpolygon();
	    v2f(p[0]);
	    v2f(p[1]);
	    v2f(p[2]);
	endpolygon();
	color(GREEN);
	cmov2(p[0][0], p[0][1]);
	charstr("vert0");
	cmov2(p[1][0], p[1][1]);
	charstr("vert1");
	cmov2(p[2][0], p[2][1]);
	charstr("vert2");
	sleep(1);
    }
    gexit();
    return 0;
}
