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
// Clock.C
//////////////////////////////////////////////////////////

#include "Clock.h"
#include <Xm/Xm.h>
#include <Xm/Scale.h>

Clock::Clock(Widget parent, char *name, int minFPS, int maxFPS ) : UIComponent(name)
{
    int middle;

    _id      = NULL;

    // Check for a valid minimum speed

    if ( minFPS < 1)
	minFPS = 1;

    // Start out in the middle of the designated range

    middle = ((maxFPS - minFPS) / 2);

    // Compute the time delta in milliseconds
    // that corresponds to the given frames per second

    _delta = 1000 / middle;    // 1/fps * 1000 milliseconds/second
    
    _w = XtVaCreateWidget( _name, xmScaleWidgetClass, parent,
			  XmNminimum,      minFPS,   
			  XmNmaximum,      maxFPS,
			  XmNvalue,        middle,
			  XmNshowValue,   TRUE,
			  NULL );

    installDestroyHandler();

    XtAddCallback ( _w, 
		   XmNvalueChangedCallback,
		   &Clock::speedChangedCallback, 
		   (XtPointer) this );
}

void Clock::start()
{
    _id      = XtAppAddTimeOut( XtWidgetToApplicationContext( _w ),
			        _delta, 
			       &Clock::timeoutCallback, 
			       (XtPointer) this);
}

void Clock::stop()
{
   if( _id )
      XtRemoveTimeOut(_id);
   _id = NULL;
}

void Clock::pulse()
{
    tick();
}

void Clock::speedChanged(int value)
{
    _delta = 1000 / value;

    if ( _id )
    {
	XtRemoveTimeOut(_id);
	_id = XtAppAddTimeOut( XtWidgetToApplicationContext( _w ),
				 _delta, 
				 &Clock::timeoutCallback, 
				 (XtPointer) this);	
    }
}

void Clock::speedChangedCallback(Widget, XtPointer clientData, XtPointer callData)
{
    XmScaleCallbackStruct *cb =  (XmScaleCallbackStruct *) callData;
    Clock * obj = (Clock *) clientData;

    obj->speedChanged(cb->value);
}


void Clock::timeoutCallback(XtPointer clientData, XtIntervalId *)
{
    Clock *obj = (Clock *) clientData;
    obj->timeout();
}

void Clock::timeout()
{
    tick();   // pure virtual function

    if ( _id )
	_id = XtAppAddTimeOut( XtWidgetToApplicationContext( _w ),
			       _delta, 
			      &Clock::timeoutCallback, 
			      (XtPointer) this);

}



