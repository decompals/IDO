#include <gl/gl.h>

#define GRAY 64
#define PINK 65

main()
{
    short red[2], green[2], blue[2];

    prefsize(400, 400);
    winopen("pink-n-gray");
    getmcolor(GRAY, &red[0], &green[0], &blue[0]); 
    getmcolor(PINK, &red[1], &green[1], &blue[1]); 
    mapcolor(GRAY, 150, 150, 150);
    mapcolor(PINK, 255, 80, 80);
    gflush();
    color(BLACK);
    clear();
    color(GRAY);
    sboxi(150, 150, 250, 250);
    color(PINK);
    circi(200, 200, 50);
    sleep(10);
    mapcolor(GRAY, red[0], green[0], blue[0]);
    mapcolor(PINK, red[1], green[1], blue[1]);
    gflush();
    gexit();
    return 0;
}
