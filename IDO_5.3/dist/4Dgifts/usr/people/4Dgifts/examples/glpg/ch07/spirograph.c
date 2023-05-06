#include <gl/gl.h>

#define PEN_TO_CENTER	0.2
#define R0 		0.35
#define R1 		0.6

void drawdot()
{
    translate(PEN_TO_CENTER, 0.0, 0.0);
    pnt2i(0, 0);
}

void draw1(theta)
float theta;
{
    pushmatrix();
	rot(theta, 'z');
	translate(R1 + R0, 0.0, 0.0);
	rot(-theta * R1 / R0, 'z');
	drawdot();
    popmatrix();
}

main()
{
    float theta;

    prefsize(400, 400);
    winopen("spirograph");
    mmode(MVIEWING);
    ortho2(-2.0, 2.0, -2.0, 2.0);
    color(BLACK);
    clear();
    color(WHITE);
    for (theta = 0.0; theta < 3600.0; theta += 0.25)
	draw1(theta);
    sleep(10);
    gexit();
    return 0;
}
