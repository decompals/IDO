#include <gl/gl.h>

main()
{
    prefsize(400, 400);
    winopen("bluebox3");
    color(BLACK);
    clear();
    color(BLUE);
    pmv2i(200, 200);
    pdr2i(200, 300);
    pdr2i(300, 300);
    pdr2i(300, 200);
    pclos();
    sleep(10);
    gexit();
    return 0;
}
