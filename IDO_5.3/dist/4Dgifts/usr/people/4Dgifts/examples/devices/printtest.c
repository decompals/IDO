/*                                                                          
 *  printconfig.c:
 * 
 *    A simple example which draws a white border around the screen and a 
 *  sequence of L's across the screen.  
 *   Useful for setting up and testing tektronics RGB color printers.
 *   Press the "Esc"[ape] key to exit program.
 *
 *                      Miq Millman - 1989
 */                                                                      

#include "gl.h"
#include "device.h"

main() {

    int dev;
    short val;

    initialize();

    while (TRUE) {
	dev = qread(&val);
	switch (dev) {
	    case ESCKEY:
		gexit();
		exit();
		break;
	    case REDRAW:
		reshapeviewport();
		drawscreen();
		break;
	}
    }
}

initialize() {

    int gid;

    noborder();
    prefposition(0,getgdesc(GD_XPMAX)-1,0,getgdesc(GD_YPMAX)-1);
    gid = winopen("printer");

    qdevice(ESCKEY);
    qenter(REDRAW,gid);

}

drawscreen() {

    pushmatrix();
    color(WHITE);    /*draws a white border around the screen, can comment out*/
    clear();
    color(BLACK);
    rectfi(1, 1, getgdesc(GD_XPMAX)-2, getgdesc(GD_YPMAX)-2);
    color(WHITE);  /*allows the text to be displayed, can comment out*/
    cmov2i(2,getgdesc(GD_YPMAX)-16);
    charstr("LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL");
    popmatrix();
}
