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

///////////////////////////////////////////////////////////////
// Stage.h
///////////////////////////////////////////////////////////////
    
#ifndef STAGE_H
#define STAGE_H
    
#include "UIComponent.h"

class Actor;
    
class Stage : public UIComponent {

 private:

    static void resizeCallback(Widget, XtPointer, XtPointer);
    static void redisplayCallback(Widget, XtPointer, XtPointer);
    
  protected:

    GC _gc;
    Dimension   _width, _height;
    Pixmap      _front, _back;

    virtual void resize();
    virtual void redisplay();
    virtual void swapBuffers();

    int _nActors;
    Actor **_cast;

  public:

    Stage(Widget, char *);    

    virtual void nextFrame();

    void addActor( Actor *);
    void removeActor( Actor *);

    virtual const char *const className() { return "Stage"; };
};

#endif
