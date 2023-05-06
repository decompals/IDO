#include <gl/gl.h>

main()
{
    prefsize(400, 400);
    winopen("bluebox2");
    color(BLACK);
    clear();
    color(BLUE);
    move2i(200, 200);
    rdr2i(0, 100);
    rdr2i(100, 0);
    rdr2i(0, -100);
    rdr2i(-100, 0);
    sleep(10);
    gexit();
    return 0;
}
