////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////
// Rotate.c++
//////////////////////////////////////////////
#include <stdio.h>
#include "Rotate.h"
#include "Box.h"
#include "installcmap.h"
#include <Xm/Frame.h>

Rotate::Rotate(const char *name) : VkSimpleWindow(name)
{
    Widget frame;

    // Put the Box in a frame just to look nice.
    
    frame = XtVaCreateManagedWidget("frame",
				    xmFrameWidgetClass,
				    mainWindowWidget(),
				    XmNshadowType, XmSHADOW_IN,
				    NULL);

    _box = new Box("box", frame);

    XtVaSetValues(_box->baseWidget(),
		  XmNwidth, 200,
		  XmNheight, 200,
		  NULL);
    _box->show();
    
    addView(frame);
}

Rotate::~Rotate()
{
    delete _box;
}

const char *Rotate::className()
{
    return "Rotate";
}

/**********************************************************************/

void Rotate::setUpWindowProperties()
{
    VkSimpleWindow::setUpWindowProperties();
    installColormap(_baseWidget, _box->baseWidget());
}
