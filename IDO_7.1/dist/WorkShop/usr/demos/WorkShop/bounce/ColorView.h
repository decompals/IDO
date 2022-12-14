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


#ifndef COLORVIEW_H
#define COLORVIEW_H

#include "UIComponent.h"

class ColorModel;

class ColorView : public UIComponent {
    
protected:
    
    ColorView ( char * );
    
public:
    
    virtual void  update( ColorModel *) = 0;
    virtual const char *const className() { return "ColorView"; }
};

#endif
