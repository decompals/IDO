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
//////////////////////////////////////////////////////
// VkGLX.c++
///////////////////////////////////////////////////////
#include "VkGLX.h"

// The GLX configuration parameter:
// 	Double buffering
//	color index (default so unspecified)
//	nothing else special

static GLXconfig defaultConfig [] = {
    { GLX_NORMAL, GLX_DOUBLE, TRUE },
    { 0, 0, 0 }
};

VkGLX::VkGLX(const char *name, Widget parent, GLXconfig glxConfig[]) : VkComponent(name)
{
    int count;
    Arg args[10];

    count = 0;

    if (glxConfig)
    {
	XtSetArg(args[count], GlxNglxConfig, glxConfig);  count++;
    }
    else
    {
	XtSetArg(args[count], GlxNglxConfig, defaultConfig);  count++;
    }

    _baseWidget = GlxCreateMDraw(parent, _name, args, count);

    XtAddCallback(_baseWidget, GlxNexposeCallback, &VkGLX::exposeCallback,
		  (XtPointer) this);
    XtAddCallback(_baseWidget, GlxNginitCallback, &VkGLX::ginitCallback,
		  (XtPointer) this);
    XtAddCallback(_baseWidget, GlxNresizeCallback, &VkGLX::resizeCallback,
		  (XtPointer) this);
    XtAddCallback(_baseWidget, GlxNinputCallback, &VkGLX::inputCallback,
		  (XtPointer) this);
}

VkGLX::~VkGLX()
{
    // Empty
}

const char * VkGLX::className()
{
  return "VkGLX";
}

void VkGLX::ginit(GlxDrawCallbackStruct *)
{
    // Empty
}

void VkGLX::expose(GlxDrawCallbackStruct *)
{
    // Empty    
}

void VkGLX::resize(GlxDrawCallbackStruct *)
{
    // Empty    
}

void VkGLX::input(GlxDrawCallbackStruct *)
{
    // Empty    
}

void VkGLX::exposeCallback(Widget, XtPointer clientData, XtPointer callData)
{
    VkGLX *obj = (VkGLX *) clientData;
    GlxDrawCallbackStruct *cb = (GlxDrawCallbackStruct *) callData;

    GLXwinset(XtDisplay(obj->baseWidget()), XtWindow(obj->baseWidget()));
    obj->expose(cb);
}

void VkGLX::ginitCallback(Widget, XtPointer clientData, XtPointer callData)
{
    VkGLX *obj = (VkGLX *) clientData;
    GlxDrawCallbackStruct *cb = (GlxDrawCallbackStruct *) callData;

    GLXwinset(XtDisplay(obj->baseWidget()), XtWindow(obj->baseWidget()));
    obj->ginit(cb);
}

void VkGLX::resizeCallback(Widget, XtPointer clientData, XtPointer callData)
{
    VkGLX *obj = (VkGLX *) clientData;
    GlxDrawCallbackStruct *cb = (GlxDrawCallbackStruct *) callData;

    GLXwinset(XtDisplay(obj->baseWidget()), XtWindow(obj->baseWidget()));
    obj->resize(cb);
}

void VkGLX::inputCallback(Widget, XtPointer clientData, XtPointer callData)
{
    VkGLX *obj = (VkGLX *) clientData;
    GlxDrawCallbackStruct *cb = (GlxDrawCallbackStruct *) callData;

    GLXwinset(XtDisplay(obj->baseWidget()), XtWindow(obj->baseWidget()));
    obj->input(cb);
}
