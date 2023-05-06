/* 
 * Define a font with three characters -- a lower-case j, 
 * an arrow, and a Greek sigma.  Use ASCII values 1 and 2 
 * ('\001' and '\002') for the arrow and sigma.  Use the 
 * ASCII value of j (= '\152') for the j character.
 */

#include <gl/gl.h>

#define EXAMPLEFONT     1

#define efont_ht        16
#define efont_nc        127
#define efont_nr	((sizeof efont_bits)/sizeof(unsigned short))

#define ASSIGN(fontch, of, wi, he, xof, yof, wid) \
    fontch.offset = of; \
    fontch.w = wi; \
    fontch.h = he; \
    fontch.xoff = xof; \
    fontch.yoff = yof; \
    fontch.width = wid

Fontchar efont_chars[efont_nc];
unsigned short efont_bits[] = {
    /* lower-case j */
    0x7000, 0xd800, 0x8c00, 0x0c00, 0x0c00, 0x0c00, 0x0c00,
    0x0c00, 0x0c00, 0x1c00, 0x0000, 0x0000, 0x0c00, 0x0c00,

    /* arrow */
    0x0200, 0x0300, 0x0380, 0xafc0, 0xafe0, 0xaff0, 0xafe0,
    0xafc0, 0x0380, 0x0300, 0x0200,

    /* sigma */
    0xffc0, 0xc0c0, 0x6000, 0x3000, 0x1800, 0x0c00, 0x0600,
    0x0c00, 0x1800, 0x3000, 0x6000, 0xc180, 0xff80,
};

main()
{
    ASSIGN(efont_chars['j'],	 0,  6, 14, 0, -2,  8);
    ASSIGN(efont_chars['\001'], 14, 12, 11, 0,  0, 14);
    ASSIGN(efont_chars['\002'], 25, 10, 13, 0,  0, 12);

    prefsize(400, 400);
    winopen("font");
    color(BLACK);
    clear();
    defrasterfont(EXAMPLEFONT, efont_ht, efont_nc,
		  efont_chars, efont_nr, efont_bits);
    font(EXAMPLEFONT);
    color(RED);
    cmov2i(100, 100);
    charstr("j\001\002\001jj\002");
    cmov2i(100, 84);
    charstr("ajb\001c\002d");
    sleep(10);
    gexit();
    return 0;
}
