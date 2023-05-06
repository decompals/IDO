#include <gl/gl.h>

main()
{
    prefsize(400, 400);
    winopen("circles");
    color(BLACK);
    clear();
    writemask(RED);
    color(RED);
    circfi(150, 250, 100);
    writemask(GREEN);
    color(GREEN);
    circfi(250, 250, 100);
    writemask(BLUE);
    color(BLUE);
    circfi(200, 150, 100);
    sleep(10);
    gexit();
    return 0;
}
