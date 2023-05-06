/*
 *	brect -
 *		Support for drawing beveled rectangles.
 *
 */
#include	<gl.h>

bulbous_rectfi(x1, y1, x2, y2)
{
    short ri, gi, bi;
    float r, g, b, h, s, v;

    getmcolor(getcolor(), &ri, &gi, &bi);
    irgb_to_rgb(ri, gi, bi, &r, &g, &b);
    rgb_to_hsv(r, g, b, &h, &s, &v);

    color(hsv(h, s, .72));
    rectfi(x1, y1, x2, y2);

    color(hsv(h, s, 0));
    move2i(x1, y1);
    draw2i(x2, y1);
    draw2i(x2, y2);
    draw2i(x1, y2);
    draw2i(x1, y1);

    color(hsv(h, s, 1));
    move2i(x1+1, y2-1);
    draw2i(x2-1, y2-1);

    color(hsv(h, s, .94));
    move2i(x1+1, y1+2);
    draw2i(x1+1, y2-2);
    move2i(x2-1, y1+2);
    draw2i(x2-1, y2-2);

    color(hsv(h, s, .9));
    move2i(x1+2, y2-2);
    draw2i(x2-2, y2-2);

    color(hsv(h, s, .84));
    move2i(x1+2, y1+3);
    draw2i(x1+2, y2-3);
    draw2i(x2-2, y2-3);
    draw2i(x2-2, y1+3);

    color(hsv(h, s, .68));
    move2i(x1+4, y1+3);
    draw2i(x2-4, y1+3);

    color(hsv(h, s, .56));
    move2i(x1+3, y1+2);
    draw2i(x2-3, y1+2);

    color(hsv(h, s, .44));
    move2i(x1+2, y1+1);
    draw2i(x2-2, y1+1);
}
