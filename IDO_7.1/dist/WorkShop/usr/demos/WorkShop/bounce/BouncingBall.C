///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//         This example code is from the book:
//
//           Object-Oriented Programming with C++ and OSF/Motif
//         by
//           Douglas Young
//           Prentice Hall, 1992
//           ISBN 0-13-630252-1	
//
//         Copyright 1991 by Prentice Hall
//         All Rights Reserved
//
//  Permission to use, copy, modify, and distribute this software for 
//  any purpose except publication and without fee is hereby granted, provided 
//  that the above copyright notice appear in all copies of the software.
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
// BouncingBall.C
//////////////////////////////////////////////////////////

#include "Application.h"
#include "Stage.h"
#include "BouncingBall.h"
#include "ColorChooser.h"
#include "InfoDialogManager.h"
#include "libc.h"

#define SIZE 25

BouncingBall::BouncingBall(Stage * stage, 
			   char *colorName ) : Actor ( stage )
{
    _delta.x = (int) (SIZE * drand48());
    _delta.y = (int) (SIZE * drand48());

    _bounds.width = _bounds.height = SIZE;
    _bounds.x = _bounds.width + 1;
    _bounds.y = _bounds.height + 1;

    _gc = NULL;

    if(colorName)	
    {
	XGCValues  gcv;
	int        scr  = DefaultScreen(theApplication->display());
	Colormap   cmap = DefaultColormap(theApplication->display(), scr);
	XColor     color, ignore;
	if(XAllocNamedColor(theApplication->display(), 
			    cmap, colorName, 
			    &color, &ignore))
	    gcv.foreground = color.pixel;
	else
	    gcv.foreground = BlackPixel(theApplication->display(), scr);

	_gc = XtGetGC(_stage->baseWidget(), 
		      GCForeground, 
		      &gcv); 
    }
    else
    {
	ColorChooser *colorChooser = new ColorChooser ( theApplication->baseWidget(), 
						       "colorChooser" );
	
	colorChooser->pickColor ( &BouncingBall::colorSelectedCallback, 
				 &BouncingBall::canceledCallback, 
				 (void *) this );
    }

}

void BouncingBall::nextFrame ( Drawable d, 
			       Dimension width, 
			       Dimension height )
{
    if( !_gc) 
	return;

    _bounds.x += _delta.x;
    _bounds.y += _delta.y;

    if ( _bounds.x + _bounds.width >= width)
    {
	_bounds.x = width - _bounds.width;
	_delta.x = -_delta.x;
    }
    else if ( _bounds.x <= 0)
    {
	_bounds.x = 0;
	_delta.x  = -_delta.x;
    }

    if ( _bounds.y + _bounds.height >= height)
    {
	_bounds.y = height - _bounds.height;
	_delta.y = -_delta.y;
    }
    else if ( _bounds.y <= 0)
    {
	_bounds.y = 0;
	_delta.y  = -_delta.y;
    }
    
    XFillArc(theApplication->display(), d, _gc, 
	     _bounds.x, _bounds.y, 
	     _bounds.width, _bounds.height, 
	     0, 360 * 64);
}



void BouncingBall::colorSelectedCallback ( int red, int green, int blue , void *clientData )
{
    BouncingBall *obj = (BouncingBall *) clientData;

    obj->colorSelected(red, green, blue);
}

void BouncingBall::colorSelected ( int red, int green, int blue)
{
    XGCValues  gcv;
    int        scr  = DefaultScreen(theApplication->display());
    Colormap   cmap = DefaultColormap(theApplication->display(), scr);
    XColor     color;    	

    color.red = red * 256;
    color.green = green * 256;
    color.blue = blue * 256 ;

    if(XAllocColor(theApplication->display(), 
		   cmap, &color))
	gcv.foreground = color.pixel;
    else
	gcv.foreground = BlackPixel(theApplication->display(), scr);
    
    _gc = XtGetGC(_stage->baseWidget(), 
		  GCForeground, 
		  &gcv); 
}


void BouncingBall::canceledCallback ( void *clientData)
{
    BouncingBall *obj = (BouncingBall *) clientData;

    theInfoDialogManager->post("No color selected, using Black as a default");
    obj->colorSelected(0, 0, 0);
}


