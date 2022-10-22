/******************************************************************************
 *
 * File:        moviescreenWin.h 
 *
 * Description: Part of moviescreen. Functions to create and access a 
 *              mixed-model GL window. 
 *
 *****************************************************************************/

#include <X11/Xlib.h>
#include <dmedia/dmedia.h>

extern DMstatus createXWindow( void );

extern Display  *getXDisplay( void );

extern Window   getXWindow( void );
