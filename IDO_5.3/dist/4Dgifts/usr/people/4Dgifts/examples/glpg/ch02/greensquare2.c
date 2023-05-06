#include <gl/gl.h>

short  vert1[3] = {200, 200, 0};
long   vert2[2] = {200, 400};
float  vert3[2] = {400.0, 400.0};
double vert4[3] = {400.0, 200.0, 0.0};

main()
{
    prefsize(400, 400);
    winopen("greensquare2");
    ortho2(100.5, 500.5, 100.5, 500.5);
    color(WHITE);
    clear();
    color(GREEN);
    bgnline();
	v3s(vert1);
	v2i(vert2);
	v2f(vert3);
	v3d(vert4);
	v3s(vert1);
    endline();
    sleep(10);
    gexit();
    return 0;
}
