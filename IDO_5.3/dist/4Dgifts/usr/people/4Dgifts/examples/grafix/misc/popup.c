/* 
 *  popup.c:
 *
 *    demonstrates "how to write your own popup menu" routines.
 *    use LEFTMOUSE instead of RIGHTMOUSE to pop up the menus.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define LINE 1
#define POINTS 2
#define CIRCLE 3
#define RECT 4
#define RECTF 5
#define QUIT 6

typedef struct {
    short type;
    char *text;
} popupentry;

popupentry mainmenu[] = {
    {LINE, "Line"},
    {POINTS, "100 points"},
    {CIRCLE, "Filled ellipse"},
    {RECT, "Outlined rectangle"},
    {RECTF, "Filled rectangle"},
    {QUIT, "Quit"},
    {0, 0}        /* mark end of menu */
};

long xmaxscrn, ymaxscrn;         /* maximum size of screen in x and y       */
Boolean MACHMODE;

main()
{
    long win;
    short val, command;


    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    prefposition(0,xmaxscrn,0,ymaxscrn);
    win = winopen("popup");

    ortho2(-1.0, 1.0, -1.0, 1.0);

    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2)/* testing to see if we're on an */
        MACHMODE = PUPDRAW;                  /* 8-bit PI or Hollywood machine */
    else {
        MACHMODE = OVERDRAW;
        overlay(2);                             /* define/setup overlays */
        gconfig();
    }

    drawmode(MACHMODE);
    mapcolor(0, 0, 0, 0);        /* background only */
    mapcolor(1, 120, 120, 120);        /* popup background */
    mapcolor(2, 255, 255, 255);        /* popup text only */
    drawmode(NORMALDRAW);

    qdevice(RIGHTMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(ESCKEY);
    tie(LEFTMOUSE, MOUSEX, MOUSEY);

    color(0);
    clear();

    while (1) {
        switch(qread(&val)) {
            case REDRAW:
                reshapeviewport();
                drawstuff(command);
                break;
            case LEFTMOUSE:
                drawstuff(command = popup(mainmenu));
                break;
            case ESCKEY:
                greset();
                gexit();
                exit(1);
		break;
            default:
                break;
        }
    }
}

drawstuff(command)
short command;
{
    register i, j;

    color(0);
    clear();
    color(GREEN);
    switch(command) {
        case LINE:
            move2(-1.0, -1.0);
            draw2(1.0, 1.0);
            break;
        case POINTS:
            for (i =  0; i < 10; i++)
               for (j = 0; j < 10; j++)
                  pnt2(i/20.0, j/20.0);
            break;
        case CIRCLE:
            circf(0.0, 0.0, 0.5);
            break;
        case RECT:
            rect(-0.5, -0.5, 0.5, 0.5);
            break;
        case RECTF:
            rectf(-0.5, -0.5, 0.5, 0.5);
            break;
        case QUIT:
            greset();
            gexit();
            exit(0);
        default:
            break;
    }
}

popup(names)
popupentry names[];
{
    register short i, menucount;
    short menutop, menubottom, menuleft, menuright;
    short lasthighlight = -1, highlight;
    short dummy, x, y;

    menucount = 0;
    qread(&x);
    qread(&y);
    pushmatrix();
    drawmode(MACHMODE); 
    ortho2(-0.5, (float)(xmaxscrn+0.5), -0.5, (float)(ymaxscrn+0.5));

    while (names[menucount].type)
        menucount++;
    menutop = y + menucount*8;
    menubottom = y - menucount*8;
    if (menutop > ymaxscrn) {
        menutop = ymaxscrn;
        menubottom = menutop - menucount*16;
    }
    if (menubottom < 0) {
        menubottom = 0;
        menutop = menubottom + menucount*16;
    }
    menuleft = x - 100;
    menuright = x + 100;
    if (menuleft < 0) {
        menuleft = 0;
        menuright = menuleft + 200;
    }
    if (menuright > xmaxscrn) {
        menuright = xmaxscrn;
        menuleft = menuright - 200;
    }

    color(0);
    clear();
    
    color(1);                /* menu background */
    rectfi(menuleft, menubottom, menuright, menutop);
    
    color(2);                /* menu text */
    move2i(menuleft, menubottom);
    draw2i(menuleft, menutop);
    draw2i(menuright, menutop);
    draw2i(menuright, menubottom);
    
    for (i = 0; i < menucount; i++) {
        move2i(menuleft, menutop - (i+1)*16);
        draw2i(menuright, menutop - (i+1)*16);
        cmov2i(menuleft + 10, menutop - 14 - i*16);
        charstr(names[i].text);
    }
    while (!qtest()) {
        x = getvaluator(MOUSEX);
        y = getvaluator(MOUSEY);
        if (menuleft < x && x < menuright && menubottom < y && y < menutop)
        {
            highlight = (menutop - y)/16;
            if (lasthighlight != -1 && lasthighlight != highlight) {
                color(1);
                rectfi(menuleft+1, menutop - lasthighlight*16 - 15,
                       menuright-1, menutop - lasthighlight*16 - 1);
                color(2);                                
                cmov2i(menuleft + 10, menutop - 14 - lasthighlight*16);
                charstr(names[lasthighlight].text);
            }
            if (lasthighlight != highlight) {
                color(2);
                rectfi(menuleft+1, menutop - highlight*16 - 15,
                       menuright-1, menutop - highlight*16 - 1);
                color(1);
                cmov2i(menuleft + 10, menutop - 14 - highlight*16);
                charstr(names[highlight].text);
            }
            lasthighlight = highlight;
        } else /* the cursor is outside the menu */ {
            if (lasthighlight != -1) {
                color(1);
                rectfi(menuleft+1, menutop - lasthighlight*16 - 15,
                       menuright-1, menutop - lasthighlight*16 - 1);
                color(2);                                
                cmov2i(menuleft + 10, menutop - 14 - lasthighlight*16);
                charstr(names[lasthighlight].text);
                lasthighlight = -1;
            }
        }
    }
    qread(&dummy);
    qread(&x);
    qread(&y);
    color(0);
    rectfi(menuleft, menubottom, menuright, menutop);
    if (menuleft<x && x<menuright && menubottom<y && y<menutop)
        x = (menutop - y)/16;
    else
        x = 0;
    drawmode(NORMALDRAW); 
    popmatrix();
    return names[x].type;
}
