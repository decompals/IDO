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
//////////////////////////////////////////////////////////////////
// VKGLX.c
/////////////////////////////////////////////////////////////////

#ifndef VK_GLX_H
#define VK_GLX_H

#include <Vk/VkComponent.h>
#include <Sgm/GlxMDraw.h>

class VkGLX : public VkComponent {

  public:

     VkGLX(const char *name, Widget parent, GLXconfig glxConfig[] = NULL);
     ~VkGLX();

     virtual const char *className();

  protected:
    
     virtual void ginit(GlxDrawCallbackStruct *cb);
     virtual void expose(GlxDrawCallbackStruct *cb);
     virtual void resize(GlxDrawCallbackStruct *cb);
     virtual void input(GlxDrawCallbackStruct *cb);

    static void exposeCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void ginitCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void resizeCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void inputCallback(Widget w, XtPointer clientData, XtPointer callData);
};

#endif
