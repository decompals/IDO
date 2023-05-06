/*
 *   text.c:
 *
 *      This program demonstrates the basics of employing a textport.  It 
 *  changes the colors and position of the wsh from which it is invoked, 
 *  opens a graphics window and then prompts for input in the wsh (now the
 *  textport).  Then it prints out what is entered in the graphics window.  
 *  Hitting the escape key (ESCKEY) closes the graphics window and returns
 *  wsh to its DEFAULT colors and position.
 *
 *                             Miq Millman - 1989
 */

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <gl/gl.h>
#include <gl/device.h>

#define MAX(a, b)	(((a) > (b)) ? (a) : (b))    

#define XORG 		200
#define YORG 		200

#define WSH_BANNER	30
#define WSH_SCROLLBAR	30

long xdim, ydim;		/* window dimensions     */
char buffer[80] = { '\0' };	/* textport input buffer */
short gid;

void init_windows();
void init_textport();
void draw_view();
void do_exit();

main() 
{
    Device dev;
    short val;
    int stdinfd, glqfd;
    int nfds;
    fd_set fdset, readfds;
    
    init_windows();
    init_textport();

    stdinfd = fileno(stdin);
    glqfd = qgetfd();
    FD_ZERO(&fdset);
    FD_SET(fileno(stdin), &fdset);
    FD_SET(glqfd, &fdset);
    nfds = MAX(stdinfd, glqfd) + 1;

    while(TRUE) {
	readfds = fdset;
	if (select(nfds, &readfds, NULL, NULL, NULL) == -1)
	    perror("text");
	else {
	    if (FD_ISSET(stdinfd, &readfds)) {
		fgets(buffer, sizeof(buffer), stdin);
		qenter(REDRAW, gid);
	    }
	    if (FD_ISSET(glqfd, &readfds)) {
		dev = qread(&val);
		switch (dev) {
		    case ESCKEY:
			if (val == 0) {
			    do_exit();
			    /* NOTREACHED */		    
			}
			break;

		    case WINQUIT:
			do_exit();
			/* NOTREACHED */		    

		    case REDRAW:
			reshapeviewport();
			draw_view();
			break;
		}
	    }
	}
    }
    /* NOTREACHED */
}


/*---------------------------------------------------------------------------
 * Initialize all windows
 *---------------------------------------------------------------------------
 */
void init_windows()
{
    xdim = getgdesc(GD_XPMAX)/2;
    ydim = getgdesc(GD_YPMAX)/2;

    /* 
     * Textports can not be used unless the GL program is run in the 
     * foreground. 
     */
    foreground();
    prefposition(XORG, XORG + xdim - 1, 
		 YORG + WSH_BANNER, YORG + ydim + WSH_BANNER - 1);
    gid = winopen("text");

    /* don't allow resizing */
    prefsize(xdim, ydim);
    winconstraints();

    qdevice(ESCKEY);
    qdevice(WINQUIT);
    qenter(REDRAW, gid);
}

/*---------------------------------------------------------------------------
 * setup textport
 *---------------------------------------------------------------------------
 */
void init_textport()
{
    /* change background color of textport */
    pagecolor(BLUE);
    /* change text color of textport */
    textcolor(MAGENTA);
    /* position textport beneath graphics port,
     * if aligning windows must consider window banner */
    textport(XORG, XORG + xdim - WSH_SCROLLBAR, 20, YORG - WSH_BANNER);
    tpon();	    /* causes textport to be popped to the top */
    printf("Move cursor over this window and type lines\n");
}

/*---------------------------------------------------------------------------
 * clear graphics window
 *---------------------------------------------------------------------------
 */
void draw_view()
{
    color(BLACK);
    clear();
    if (strlen(buffer)) {
	color(WHITE);
	cmov2i((xdim - strwidth(buffer))/2, ydim/2);
	charstr(buffer);
    }
}


/*---------------------------------------------------------------------------
 * exit gracefully 
 *---------------------------------------------------------------------------
 */
void do_exit()
{
    textinit();        /* reset textport */
    gexit();
    exit(0);
    /* NOTREACHED */		    
}
