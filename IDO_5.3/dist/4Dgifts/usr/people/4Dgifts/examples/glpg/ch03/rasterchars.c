#include <gl/gl.h>

main()
{
    prefsize(400, 400);
    winopen("rasterchars");
    color(BLACK);
    clear();
    color(RED);
    cmov2i(50, 80);
    charstr("The first line is drawn ");
    charstr("in two parts. ");
    cmov2i(50, 80 - 14);
    charstr("This line is 14 pixels lower.");
    sleep(10);
    gexit();
    return 0;
}
