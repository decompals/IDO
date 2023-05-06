#include <stdio.h>
#include <gl/gl.h>

#define WINSIZE     400
#define RGB_BLACK   0x000000
#define RGB_RED	    0x0000ff
#define RGB_GREEN   0x00ff00
#define RGB_BLUE    0xff0000

main()
{
    if (getgdesc(GD_BITS_NORM_SNG_RED) == 0) {
	fprintf(stderr, "Single buffered RGB not available\n");
	return 1;
    }
    if (getgdesc(GD_BLEND) == 0) {
	fprintf(stderr, "Blending not available\n");
	return 1;
    }
    prefsize(WINSIZE, WINSIZE);
    winopen("blendcircs");
    mmode(MVIEWING);
    RGBmode();
    gconfig();
    mmode(MVIEWING);
    ortho2(-1.0, 1.0, -1.0, 1.0);
    glcompat(GLC_OLDPOLYGON, 0);
    blendfunction(BF_SA, BF_ONE);
    cpack(RGB_BLACK);
    clear();
    cpack(0x80000000 | RGB_RED);	    /* red with alpha = 128/255 */
    circf(0.25, 0.0, 0.7);
    sleep(2);
    cpack(0x4f000000 | RGB_GREEN);	    /* green with alpha = 79/255 */
    circf(-0.25, 0.25, 0.7);
    sleep(2);
    cpack(0x30000000 | RGB_BLUE);	    /* blue with alpha = 48/255 */
    circf(-0.25, -0.25, 0.7);
    sleep(10);
    gexit();
    return 0;
}
