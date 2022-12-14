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


///////////////////////////////////////////////////////////
// ColorChooser.h
//////////////////////////////////////////////////////////
#ifndef COLORCHOOSER_H
#define COLORCHOOSER_H

#include "UIComponent.h"

typedef void (*ColorSelectedCallback)( int, int, int ,void *);
typedef void (*CanceledCallback)( void *);

class    ColorModel;
class    ColorView;

class ColorChooser : public UIComponent {
    
private:
    
    ColorModel    *_model;
    ColorView     *_rgbSliders;
    ColorView     *_swatch;
    ColorView     *_rgbView;
    ColorView     *_hsvView;

    void  *_clientData;

    Widget _okButton;
    Widget _cancelButton;
    
    ColorSelectedCallback _okCallback;
    CanceledCallback _canceledCallback;
    
    void   ok();
    void   canceled();
    
    static void okCallback ( Widget, 
                            XtPointer, 
                            XtPointer );

    static void canceledCallback ( Widget, 
				  XtPointer, 
				  XtPointer );
    
public:
    
    ColorChooser ( Widget , char *);
    virtual ~ColorChooser ();
    void pickColor( ColorSelectedCallback, CanceledCallback, void *);
    
    virtual const char *const className() { return "ColorChooser"; }
};
#endif



