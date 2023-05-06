#include <gl/gl.h>

long vert1[2] = {101, 101};	/* lower left corner */
long vert2[2] = {101, 500};	/* upper left corner */
long vert3[2] = {500, 500};	/* upper right corner */
long vert4[2] = {500, 101};	/* lower right corner */

main()
{
    prefsize(400, 400);
    winopen("crisscross");
    ortho2(100.5, 500.5, 100.5, 500.5);
    color(WHITE);
    clear();
    color(RED);
    bgnline();
	v2i(vert1);
	v2i(vert3);
    endline();
    bgnline();
	v2i(vert2);
	v2i(vert4);
    endline();
    sleep(10);
    gexit();
    return 0;
}
