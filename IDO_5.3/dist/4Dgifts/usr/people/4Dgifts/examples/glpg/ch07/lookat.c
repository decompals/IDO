#include <gl/gl.h>

long v[8][3] = {
    {-1, -1, -1},
    {-1, -1,  1},
    {-1,  1,  1},
    {-1,  1, -1},
    { 1, -1, -1},
    { 1, -1,  1},
    { 1,  1,  1},
    { 1,  1, -1},
};
int path[16] = {
    0, 1, 2, 3,
    0, 4, 5, 6,
    7, 4, 5, 1,
    2, 6, 7, 3
};

void drawcube()
{
    int i;

    bgnline();
    for (i = 0; i < 16; i++)
	v3i(v[path[i]]);
    endline();
}

main()
{
    prefsize(600, 400);
    winopen("lookat");
    mmode(MVIEWING);
    perspective(300, 1.5, 0.1, 10.0);
    lookat(5.0, 4.0, 6.0, 1.0, 1.0, 1.0, 0);
    color(BLACK);
    clear();
    color(WHITE);
    drawcube();
    sleep(10);
    gexit();
    return 0;
}
