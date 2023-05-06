#include <gl/gl.h>

main()
{
    prefsize(400, 400);
    winopen("bluebox");
    color(BLACK);
    clear();
    color(BLUE);
    move2i(200, 200);
    draw2i(200, 300);
    draw2i(300, 300);
    draw2i(300, 200);
    draw2i(200, 200);
    sleep(10);
    gexit();
    return 0;
}
