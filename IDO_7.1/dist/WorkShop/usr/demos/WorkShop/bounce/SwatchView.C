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


#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include "SwatchView.h"
#include "ColorModel.h"


SwatchView::SwatchView ( Widget parent, char *name ) : ColorView ( name )
{
    int    status;
    Pixel  pixels[1];
    
    _w = XtCreateWidget(_name, xmFrameWidgetClass, parent, NULL, 0);
    
    installDestroyHandler();    
    
    status = XAllocColorCells( XtDisplay(_w),
                              DefaultColormapOfScreen(XtScreen(_w)),
                              FALSE,
                              NULL, 
                              0,
                              pixels, 1);
    
    if ( status == FALSE )
        XtError("Can't allocate color cell");
    
    _index = pixels[0];
    
    _swatch = XtVaCreateWidget("swatch", 
                               xmDrawingAreaWidgetClass, 
                               _w, 
                               XmNbackground, _index, 
                               NULL );
    XtManageChild(_swatch);
}

void SwatchView::update( ColorModel *model )
{
    if ( _swatch )
    {   
        XColor color;
        
        color.red    = model->red()   * 256;
        color.green  = model->green() * 256;
        color.blue   = model->blue()  * 256;
        color.flags  = DoRed | DoBlue | DoGreen;
        color.pixel  = _index;
        
        XStoreColor(XtDisplay(_w),
                    DefaultColormapOfScreen(XtScreen(_w)),
                    &color);
    }
}

