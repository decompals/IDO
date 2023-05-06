#include <gl/gl.h>

long vert1[2] = {200, 200};
long vert2[2] = {200, 400};
long vert3[2] = {400, 400};
long vert4[2] = {400, 200};

main()
{
    prefsize(400, 400);
    winopen("greensquare");
    ortho2(100.5, 500.5, 100.5, 500.5);
    color(WHITE);
    clear();
    color(GREEN);
    bgnline();
	v2i(vert1);
	v2i(vert2);
	v2i(vert3);
	v2i(vert4);
	v2i(vert1);
    endline();
    sleep(10);
    gexit();
    return 0;
}
