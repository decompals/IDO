#include <gl/gl.h>

main()
{
    prefsize(400, 400);
    winopen("piechart");
    ortho2(-1.0, 1.0, -1.0, 1.0);
    color(BLACK);
    clear();
    color(RED);
    arcf(0.0, 0.0, 0.9, 0, 800);
    color(GREEN);
    arcf(0.0, 0.0, 0.9, 800, 1200);
    color(YELLOW);
    arcf(0.0, 0.0, 0.9, 1200, 2200);
    color(MAGENTA);
    arcf(0.0, 0.0, 0.9, 2200, 3400);
    color(BLUE);
    arcf(0.0, 0.0, 0.9, 3400, 0);
    sleep(10);
    gexit();
    return 0;
}
