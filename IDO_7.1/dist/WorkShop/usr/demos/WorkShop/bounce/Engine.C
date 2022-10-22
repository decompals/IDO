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
// Engine.C
//////////////////////////////////////////////////////////

#include "Application.h"
#include "Stage.h"
#include "Engine.h"
#include "ColorChooser.h"
#include "InfoDialogManager.h"
#include "libc.h"

String          colors[] = {"gray", "lightseagreen",
			    "dimgrey", "forestgreen", "black",
			    "red", "cyan", "firebrick"};

#define	PI	3.141592654

Engine::Engine(Stage * stage ) : Actor ( stage )
{
    XColor          srcColor, dummyColor;
    
    gcPiston = XCreateGC(theApplication->display(), XtWindow(theApplication->baseWidget()),
			 (unsigned long) 0, NULL);
    XAllocNamedColor(theApplication->display(),
		     DefaultColormap(theApplication->display(), 0),
		     colors[0], &srcColor, &dummyColor);
    XSetForeground(theApplication->display(), gcPiston, srcColor.pixel);

    gcShaft = XCreateGC(theApplication->display(), XtWindow(theApplication->baseWidget()), (unsigned long) 0, NULL);
    XAllocNamedColor(theApplication->display(), DefaultColormap(theApplication->display(), 0), colors[1], &srcColor, &dummyColor);
    
    XSetForeground(theApplication->display(), gcShaft, srcColor.pixel);
    XSetLineAttributes(theApplication->display(), gcShaft, 2, LineSolid, CapButt, JoinMiter);
    
    gcCylinder = XCreateGC(theApplication->display(), XtWindow(theApplication->baseWidget()), (unsigned long) 0, NULL);
    XAllocNamedColor(theApplication->display(), DefaultColormap(theApplication->display(), 0), colors[2], &srcColor, &dummyColor);
    
    XSetForeground(theApplication->display(), gcCylinder, srcColor.pixel);
    
    gcRoter = XCreateGC(theApplication->display(), XtWindow(theApplication->baseWidget()), (unsigned long) 0, NULL);
    XAllocNamedColor(theApplication->display(), DefaultColormap(theApplication->display(), 0), colors[3], &srcColor, &dummyColor);
    
    XSetForeground(theApplication->display(), gcRoter, srcColor.pixel);
    
    gcBack = XCreateGC(theApplication->display(), XtWindow(theApplication->baseWidget()), (unsigned long) 0, NULL);
    XAllocNamedColor(theApplication->display(), DefaultColormap(theApplication->display(), 0), colors[4], &srcColor, &dummyColor);
    
    XSetForeground(theApplication->display(), gcBack, srcColor.pixel);
    
    gcDep = XCreateGC(theApplication->display(), XtWindow(theApplication->baseWidget()), (unsigned long) 0, NULL);
    XAllocNamedColor(theApplication->display(), DefaultColormap(theApplication->display(), 0), colors[5], &srcColor, &dummyColor);
    
    XSetForeground(theApplication->display(), gcDep, srcColor.pixel);
    
    gcPre = XCreateGC(theApplication->display(), XtWindow(theApplication->baseWidget()), (unsigned long) 0, NULL);
    XAllocNamedColor(theApplication->display(), DefaultColormap(theApplication->display(), 0), colors[6], &srcColor, &dummyColor);
    
    XSetForeground(theApplication->display(), gcPre, srcColor.pixel);
    
    gcEngine = XCreateGC(theApplication->display(), XtWindow(theApplication->baseWidget()), (unsigned long) 0, NULL);
    XAllocNamedColor(theApplication->display(), DefaultColormap(theApplication->display(), 0), colors[7], &srcColor, &dummyColor);
    
    XSetForeground(theApplication->display(), gcEngine, srcColor.pixel);
    
}

void Engine::nextFrame ( Drawable d, 
			       Dimension width, 
			       Dimension height )
{
    static int      sw = 1;
    static double    angle = 180, theta;
    double          piston_x1, piston_y1;
    double          piston_x2, piston_y2;
    
    sw ^= 1;

    angle += 18;

    if(angle >= 180)
	angle = -180;
    
    theta = angle * (PI / 180.0);
    piston_x1 = sin(theta) * (width * 0.1) + (width / 2);
    piston_y1 = cos(theta) * (height * 0.1) + (height * 0.7);
    piston_x2 = width / 2;
    piston_y2 = piston_y1 - (height * 0.3);
    
    /* Draw heat sink */
    
    XFillRectangle(theApplication->display(), d, gcEngine,
		   (int) (width * 0.35), (int) (height * 0.3),
		   (int) (width * 0.3), (int) (height * 0.02));
    
    XFillRectangle(theApplication->display(), d, gcEngine,
		   (int) (width * 0.35), (int) (height * 0.35),
		   (int) (width * 0.3), (int) (height * 0.02));
    
    XFillRectangle(theApplication->display(), d, gcEngine,
		       (int) (width * 0.35), (int) (height * 0.4),
		   (int) (width * 0.3), (int) (height * 0.02));
    
    /* Draw Cylinder */
    
    XFillRectangle(theApplication->display(), d, gcEngine,
		   (int) (width * 0.42), (int) (height * 0.24),
		   (int) (width * 0.16), (int) (height * 0.5));
    
    XFillArc(theApplication->display(), d, gcEngine,
		 (int) (piston_x2 - (width * 0.15)), (int) ((height * 0.7) - (height * 0.15)), (int) (width * 0.3), (int) (height * 0.3), 0 * 64, 360 * 64);
    
    XFillArc(theApplication->display(), d, gcBack,
	     (int) (piston_x2 - (width * 0.14)), (int) ((height * 0.7) - (height * 0.14)), (int) (width * 0.28), (int) (height * 0.28), 0 * 64, 360 * 64);
    
    XFillRectangle(theApplication->display(), d, gcBack,
		   (int) (width * 0.43), (int) (height * 0.25),
		   (int) (width * 0.14), (int) (height * 0.5));
    
    /* Draw Gas */
    
    if (sw != 0) {
	XFillRectangle(theApplication->display(), d, gcPre,
		       (int) (width * 0.435), (int) (height * 0.25),
		       (int) (width * 0.13), (int) (piston_y2 - (height * 0.05) - (height * 0.26) + (height * 0.1)));
    } else {
	XFillRectangle(theApplication->display(), d, gcDep,
		       (int) (width * 0.435), (int) (height * 0.25),
		       (int) (width * 0.13), (int) (piston_y2 - (height * 0.05) - (height * 0.26) + (height * 0.1)));
    }
    
    /* Draw Piston arm */
    XDrawLine(theApplication->display(), d, gcShaft,
	      (int) piston_x1, (int) piston_y1, (int) piston_x2, (int) piston_y2);
    XDrawLine(theApplication->display(), d, gcShaft,
	      (int) piston_x1, (int) piston_y1, (int) piston_x2, (int) (height * 0.7));
    
    
    /* Draw Piston & ring */
    
    XFillRectangle(theApplication->display(), d, gcPiston,
		   (int) (width * 0.435), (int) (piston_y2 - (height * 0.05)),
		   (int) (width * 0.13), (int) (height * 0.1));
    
    XFillRectangle(theApplication->display(), d, gcShaft,
		   (int) (width * 0.435), (int) (piston_y2 - (height * 0.045)),
		   (int) (width * 0.13), (int) (height * 0.01));
	
    XFillArc(theApplication->display(), d, gcShaft,
	     (int) (piston_x2 - (width * 0.015)), (int) (piston_y2 - (height * 0.015)), (int) (width * 0.03), (int) (height * 0.03), 0, 360 * 64);
    
    /* Draw Roter */
    XFillArc(theApplication->display(), d, gcRoter,
	     (int) (piston_x2 - (width * 0.1)), (int) ((height * 0.7) - (height * 0.1)), (int) (width * 0.2), (int) (height * 0.2), (int) (angle + 30) * 64, 120 * 64);
    
    XFillArc(theApplication->display(), d, gcPiston,
	     (int) (piston_x2 - (width * 0.025)), (int) ((height * 0.7) - (height * 0.025)), (int) (width * 0.05), (int) (height * 0.05), 0, 360 * 64);
    
}



