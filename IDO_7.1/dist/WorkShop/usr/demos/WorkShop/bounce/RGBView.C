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


#include "RGBView.h"
#include "ColorModel.h"
#include <Xm/Xm.h>
#include <Xm/Text.h>

#include <stdio.h>


RGBView::RGBView(Widget parent, char *name ) : 
TextView(parent, name)
{
    // Empty
}

void RGBView::update( ColorModel *model)
{
    char buf[100];      
    
    sprintf(buf, "%3.3d", model->red());
    XmTextSetString(_field1, buf);
    sprintf(buf, "%3.3d", model->green());
    XmTextSetString(_field2, buf);
    sprintf(buf, "%3.3d", model->blue());
    XmTextSetString(_field3, buf);
}

