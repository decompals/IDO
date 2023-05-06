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

/////////////////////////////////////////////////////////////////////
// resizer.c++: Test of the VkResizer component
////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkResizer.h>

#include <Xm/PushB.h>
#include <Xm/BulletinB.h>

#include <iostream.h>
#include <stdlib.h>

class SampleWindow : public VkSimpleWindow {

  private:

     static void activateCallback(Widget, XtPointer, XtPointer);


     VkResizer *_resizer;

     void activate(Widget w, XtPointer callData);
     void resize(VkCallbackObject *, void *, void *);

  public:

      SampleWindow(const char *name);
      ~SampleWindow();
};

SampleWindow::SampleWindow(const char *name) : VkSimpleWindow(name)
{
   if (theApplication->argc() == 6 && *theApplication->argv(5) != '0') 
     _resizer = new VkResizer(False, True);
   else
    _resizer = new VkResizer();

   VkAddCallbackMethod(VkResizer::stateChangedCallback, _resizer, this, &SampleWindow::resize, NULL);



   Widget bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, mainWindowWidget(), 
				       XmNwidth,        200,
				       XmNheight,       200,
				       XmNmarginWidth,  0,
				       XmNmarginHeight, 0,
				       XmNresizePolicy, XmRESIZE_NONE,
				       NULL);

  Widget button1 = XtVaCreateManagedWidget("push to resize", xmPushButtonWidgetClass, bb, 
					 XmNx, 50,
					 XmNy, 50,
					 NULL);
					 
  XtAddCallback(button1, 
		XmNactivateCallback,
		SampleWindow::activateCallback, 
		(XtPointer) this);


  Widget button2 = XtVaCreateManagedWidget("push to resize", xmPushButtonWidgetClass, bb, 
					 XmNx, 50,
					 XmNy, 120,
					 NULL);
	   
   XtAddCallback(button2, 
		 XmNactivateCallback,
		 SampleWindow::activateCallback, 
		 (XtPointer) this);

   if (theApplication->argc() >= 5) 
    _resizer->setIncrements(atoi(theApplication->argv(1)),
			   atoi(theApplication->argv(2)),
			   atoi(theApplication->argv(3)),
			   atoi(theApplication->argv(4)));

   addView(bb);
}

SampleWindow::~SampleWindow()
{
  delete _resizer;
}

void SampleWindow::activateCallback(Widget w, XtPointer clientData, XtPointer callData)
{
  SampleWindow *obj = (SampleWindow *) clientData;

  obj->activate(w, callData);
}

void SampleWindow::activate(Widget w, XtPointer)
{
  if (_resizer->baseWidget() != w) 
  {
    _resizer->attach(w);
    _resizer->show();
  }
  else if (_resizer->shown()) 
    _resizer->hide();
  else
    _resizer->show();
}

void SampleWindow::resize(VkCallbackObject *, void *, void *callData)
{
    VkResizerReason reason = (VkResizerReason) callData;

   switch (reason) 
   {
   case VR_resizing:
     cout <<  "Resizing\n" << flush;
     break;

  case VR_moving:
     cout <<  "Moving\n" << flush;
     break;

  case VR_resized:
     cout <<  "Resized\n" << flush;
     break;

  case VR_moved:
     cout <<  "Moved\n" << flush;
     break;

  case VR_none:
    break;
  }
}

void main(int argc, char **argv)
{
  VkApp *app = new VkApp("Resizer", &argc, argv);

  SampleWindow *win = new SampleWindow("resizer");

  win->show();
  app->run();
}
