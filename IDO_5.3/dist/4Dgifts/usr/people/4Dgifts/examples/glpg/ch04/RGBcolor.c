#include <stdio.h>
#include <gl/gl.h>

main()
{
    if (getgdesc(GD_BITS_NORM_SNG_RED) == 0) {
	fprintf(stderr, "Single buffered RGB not available on this machine\n");
	return 1;
    }
    prefsize(400, 400);
    winopen("RGBcolor");
    RGBmode();
    gconfig();
    RGBcolor(0, 100, 200);
    clear();
    sleep(10);
    gexit();
    return 0;
}
