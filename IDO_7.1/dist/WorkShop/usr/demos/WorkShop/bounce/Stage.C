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
// in supporting documentation.
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
// Stage.C
//////////////////////////////////////////////////////////
#include "Application.h"
#include "Stage.h"
#include "Actor.h"
#include <Xm/DrawingA.h>

Stage::Stage ( Widget parent, char *name ) : UIComponent( name )
{
    XGCValues  gcv;

    _w =  XtCreateWidget ( _name, 
			  xmDrawingAreaWidgetClass,
			  parent,
			  NULL, 0 );

    installDestroyHandler();

    XtAddCallback ( _w, XmNresizeCallback, &Stage::resizeCallback,    (XtPointer) this );
    XtAddCallback ( _w, XmNexposeCallback, &Stage::redisplayCallback, (XtPointer) this );

    XtVaGetValues(_w, 
		  XmNbackground, &gcv.foreground,
		  NULL );

    _front = NULL;
    _back  = NULL;
    _nActors = 0;	
    _cast = NULL;

    _gc = XtGetGC(_w, GCForeground, &gcv); 

    resize();
}


void Stage::redisplayCallback ( Widget, 
			       XtPointer clientData, 
			       XtPointer )
{

    Stage *obj = (Stage *) clientData;
    obj->redisplay();
}    


void Stage::redisplay ( )
{
    XCopyArea(theApplication->display(), _front, 
	      XtWindow(_w), _gc, 0, 0, 
	      _width, _height, 0, 0);
}    


void Stage::resizeCallback ( Widget, 
			    XtPointer clientData, 
			    XtPointer )
{
    Stage *obj = (Stage *) clientData;
    obj->resize();

}    

void Stage::resize()
{
    XtVaGetValues(_w, XmNwidth,  &_width,
		  XmNheight,     &_height,
		  NULL );

  if(_width == 0 || _height == 0)
	return;

    if(_front)	
	XFreePixmap(XtDisplay(_w), _front);

    if(_back)	
	XFreePixmap(XtDisplay(_w), _back);

    _back = XCreatePixmap(XtDisplay(_w),
			  DefaultRootWindow(XtDisplay(_w)),
			  _width, _height, 
			  DefaultDepthOfScreen(XtScreen(_w)));

    _front = XCreatePixmap(XtDisplay(_w),
			   DefaultRootWindow(XtDisplay(_w)),
			   _width, _height, 
			   DefaultDepthOfScreen(XtScreen(_w)));

    XFillRectangle(XtDisplay(_w), _back, _gc, 0, 0, _width, _height);
    XFillRectangle(XtDisplay(_w), _front, _gc, 0, 0, _width, _height);
}


void Stage::addActor ( Actor *newActor )
{
   int i;

   Actor **newList;

    newList = new Actor*[_nActors + 1];

    for(i=0; i < _nActors; i++)
	newList[i] = _cast[i];

    newList[_nActors] = newActor;
    delete _cast;
    _cast = newList;
    _nActors++;
}


void Stage::removeActor( Actor *oldActor )
{
   int i, j;
   Boolean found = FALSE;
   Actor **newList;

   // remove the Actor from the list

   for ( i = 0; i < _nActors; i++ )
       if ( _cast[i] == oldActor )
       {
	   found = TRUE;
	   for ( j = i; j < _nActors-1; j++ )
	       _cast[j] = _cast[j+1];
	   break;
       }

   if ( !found )
       return;

   _nActors--;
    
   newList = new Actor*[_nActors];

   for ( i = 0; i < _nActors; i++ )
       newList[i] = _cast[i];

   delete _cast;
   _cast = newList;
}

void Stage::swapBuffers()
{
    if(XtIsRealized(_w))
    {
	Pixmap tmp;

	tmp    = _front;
	_front = _back;
	_back  = tmp;

	XCopyArea(XtDisplay(_w), _front, XtWindow(_w) , _gc, 0, 0, _width, _height, 0, 0);
	XFillRectangle(XtDisplay(_w), _back, _gc, 0, 0, _width, _height);
    }
}

void  Stage::nextFrame()
{

    int i;

    swapBuffers();

    for( i = 0; i < _nActors; i++)
	_cast[i]->nextFrame(_back, _width, _height);

}






