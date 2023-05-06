#include <gl/gl.h>

Icoord parray[6][2] = {
    {100,  0},
    {  0, 200},
    {100, 400},
    {300, 400},
    {400, 200},
    {300,   0}
};

main()
{
    prefsize(400, 400);
    winopen("hexagon");
    color(BLACK);
    clear();
    color(GREEN);
    polf2i(6, parray);
    sleep(10);
    gexit();
    return 0;
}
