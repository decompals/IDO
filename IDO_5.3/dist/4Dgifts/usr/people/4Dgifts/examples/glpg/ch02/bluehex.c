#include <gl/gl.h>

float hexdata[6][2] ={
    {20.0, 10.0},
    {10.0, 30.0},
    {20.0, 50.0},
    {40.0, 50.0},
    {50.0, 30.0},
    {40.0, 10.0}
};

main()
{
    int i;

    prefsize(400, 400);
    winopen("bluehex");
    ortho2(0.0, 60.0, 0.0, 60.0);
    color(BLACK);
    clear();
    color(BLUE);
    bgnpolygon();
    for (i = 0; i < 6; i++)
	v2f(hexdata[i]);
    endpolygon();
    sleep(10);
    gexit();
    return 0;
}
