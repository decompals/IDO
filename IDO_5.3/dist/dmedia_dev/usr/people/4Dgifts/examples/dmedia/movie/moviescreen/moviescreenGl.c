/******************************************************************************
 *
 * File:        moviescreenGl.c 
 *
 * Description: Code used by moviescreen for determining the movie window 
 *              position, and erasing the movie window as it moves via GL 
 *              drawing.
 *
 * Functions:   SGI Movie Library functions used:
 *
 *              mvSetViewOffset()
 *              mvFindTrackByMedium()
 *              mvGetImageWidth()
 *              mvGetImageHeight()
 *              mvGrabIrisGL()
 *              mvReleaseIrisGL()
 *
 *****************************************************************************/

#include <dmedia/moviefile.h>
#include <dmedia/movieplay.h>
#include "moviescreenWin.h"
#include "moviescreenArgs.h"
#include "moviescreenGl.h"
#include <X11/Xlib.h>
#include <gl/glws.h>
#include <stdio.h>
#include <dmedia/dmedia.h>

static int      offsetx = 0;
static int      offsety = 0;
static int      xmax    = 0;
static int      ymax    = 0;
static int      dx      = 0;
static int      dy      = 0;

/*********
 *
 * Initialize position, direction vectors to random values.
 *
 *********/

void initPositionAndDirection( int zoom, int width, int height )
{
    Display *dpy = getXDisplay();

    xmax = ( DisplayWidth( dpy, DefaultScreen( dpy ) ) - ( width * zoom ) );
    ymax = ( DisplayHeight( dpy, DefaultScreen( dpy ) ) - ( height * zoom ) );
    offsetx = random() % xmax;
    offsety = random() % ymax;

    dx = ( random() % 2 ) ? -1 : 1;
    dy = ( random() % 2 ) ? -1 : 1;
}

/*********
 *
 * Return the x offset of the movie within the window.
 *
 *********/

int getXOffset()
{
    return offsetx;
}

/*********
 *
 * Return the y offset of the movie within the window.
 *
 *********/

int getYOffset()
{
    return offsety;
}

/*********
 *
 * Move the movie region within the window.
 *
 *********/

void moveSaverPicture()
{
    MVid theMovie = getMovieID();

    if ( isFullScreen() )
        return;

    offsetx += dx;
    if ( offsetx < 0 ) {
        offsetx = 0;
        dx = -dx;
    }
    if ( offsetx > xmax ) {
        offsetx = xmax;
        dx = -dx;
    }

    offsety += dy;
    if ( offsety < 0 ) {
        offsety = 0;
        dy = -dy;
    }
    if ( offsety > ymax ) {
        offsety = ymax;
        dy = -dy;
    }

    mvSetViewOffset( theMovie, offsetx, offsety, DM_TRUE );
}

/*********
 *
 * Erase the movie's trail as it moves around the window.
 *
 *********/

void undrawSaverPicture()
{
    int  width;
    int  height;
    MVid imageTrack;
    MVid theMovie = getMovieID();

    if ( isFullScreen() )
        return;

    /*
     * Determine current size of movie on display
     */

    mvFindTrackByMedium( theMovie, DM_IMAGE, &imageTrack );
    width  = mvGetImageWidth( imageTrack );
    height = mvGetImageHeight( imageTrack );
    width *= getZoom();
    height *= getZoom();

    /*
     * Draw two black boxes to erase trailing garbage as movie dances
     * around the display.
     */

    mvGrabIrisGL();         /* or else all hell will break loose. */

    bgnpolygon();

    {
	short vctr[2];
        if ( dx > 0 ) {

	    vctr[0] = offsetx - 1;
	    vctr[1] = offsety - 1;
            v2s( vctr );

	    vctr[0] = offsetx +dx;
	    vctr[1] = offsety - 1;
            v2s( vctr );

	    vctr[0] = offsetx + dx;
	    vctr[1] = offsety + height + 1;
            v2s( vctr );

	    vctr[0] = offsetx - 1;
	    vctr[1] = offsety + height + 1;
            v2s( vctr );

        } else {

	    vctr[0] = offsetx + width;
	    vctr[1] = offsety - 1;
            v2s( vctr );

	    vctr[0] = offsetx + width + dx + 1;
	    vctr[1] = offsety - 1;
            v2s( vctr );

	    vctr[0] = offsetx + width + dx + 1;
	    vctr[1] = offsety + height + 1;
            v2s( vctr );

	    vctr[0] = offsetx + width;
	    vctr[1] = offsety + height + 1;
            v2s( vctr );
        }
        if ( dy > 0 ) {

	    vctr[0] = offsetx - 1;
	    vctr[1] = offsety - 1;
            v2s( vctr );

	    vctr[0] = offsetx + width + 1;
	    vctr[1] = offsety - 1;
            v2s( vctr );

	    vctr[0] = offsetx + width + 1;
	    vctr[1] = offsety + dy;
            v2s( vctr );

	    vctr[0] = offsetx - 1;
	    vctr[1] = offsety + dy;
            v2s( vctr );

        } else {

	    vctr[0] = offsetx - 1;
	    vctr[1] = offsety + height;
            v2s( vctr );

	    vctr[0] = offsetx + width + 1;
	    vctr[1] = offsety + height;
            v2s( vctr );

	    vctr[0] = offsetx + width + 1;
	    vctr[1] = offsety + height + dy + 1;
            v2s( vctr );

	    vctr[0] = offsetx - 1;
	    vctr[1] = offsety + height + dy + 1;
            v2s( vctr );
        }
    }
    endpolygon();

    mvReleaseIrisGL();
}
