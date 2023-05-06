/* 
 *    popuputils.c
 *
 *     used by trans.c to do home-made popup menus the same as popup.c
 */

#include <gl/gl.h>
#include <gl/device.h>
#include "popup.h"

initpopup()
{
    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2)
        MACHMODE = PUPDRAW;
    else {
        MACHMODE = OVERDRAW;
        overlay(2);                             /* define/setup overlays */
        gconfig();
    }
    drawmode(MACHMODE);
    mapcolor(0, 0, 0, 0);
    mapcolor(1, 120, 120, 120);
    mapcolor(2, 255, 255, 255);
    drawmode(NORMALDRAW);

    qdevice(LEFTMOUSE);
    tie(LEFTMOUSE, MOUSEX, MOUSEY);
}

popup(names)
popupentry names[];
{
    register short i, menucount;
    short menutop, menubottom, menuleft, menuright;
    short lasthighlight = -1, highlight;
    long  sizex, sizey;
    short dummy, x, y;

    menucount = 0;
    qread(&x);
    qread(&y);
    pushmatrix();
    fullscrn();
    getsize(&sizex,&sizey); /* get the new window size */
    ortho2 (0.0, (float) sizex, 0.0, (float) sizey); /* set the new ortho */
    drawmode(MACHMODE); 

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

    cursoff();
    color(0);
    clear();
    
    color(1);		/* menu background */
    rectfi(menuleft, menubottom, menuright, menutop);
    
    color(2);		/* menu text */
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
    curson();
    while (!qtest()) {
	x = getvaluator(MOUSEX);
	y = getvaluator(MOUSEY);
	if (menuleft < x && x < menuright && menubottom < y && y < menutop)
	{
	    highlight = (menutop - y)/16;
	    cursoff();
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
	    curson();
	   } else /* the cursor is outside the menu */ {
	       if (lasthighlight != -1) {
		  cursoff();
		  color(1);
		  rectfi(menuleft+1, menutop - lasthighlight*16 - 15,
		       menuright-1, menutop - lasthighlight*16 - 1);
		  color(2);				
		  cmov2i(menuleft + 10, menutop - 14 - lasthighlight*16);
		  charstr(names[lasthighlight].text);
		  curson();
		  lasthighlight = -1;
	    }
        }
    }
    qread(&dummy);
    qread(&x);
    qread(&y);
    color(0);
    cursoff();
    rectfi(menuleft, menubottom, menuright, menutop);
    curson();
    if (menuleft<x && x<menuright && menubottom<y && y<menutop)
	x = (menutop - y)/16;
    else
	x = 0;
    drawmode(NORMALDRAW); 
    endfullscrn();
    popmatrix();
    return names[x].type;
}
