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


////////////////////////////////////////////////////////////////////
// sample2_service.c++: demonstrate round-trip tooltalk requests
//
// To see this demo, build and run sample2_client
////////////////////////////////////////////////////////////////////

#include <Vk/VkMsgFacility.h>
#include <Vk/VkMsgWindow.h>
#include <Vk/VkMsgApp.h>
#include <Vk/VkSubMenu.h>
#include <Vk/VkErrorDialog.h>
#include <Vk/VkFormat.h>
#include <Xm/Scale.h>

#define MESSAGE "sample2_value"

class SampleWindow : public VkMsgWindow {

  public:

    SampleWindow(const char *name);
    ~SampleWindow();

  protected:

    Widget _recieveSlider;

  private:

    static Boolean messageHandler(void               *client_data,
				  VkMsgFacilityReason reason,
				  VkMsgMessage        msg_in,
				  char               *op, 
				  int                 argc, 
				  VkMsgArg           *argv);

    static void quitCallback(Widget, 
			     XtPointer,
			     XtPointer);
};


SampleWindow::SampleWindow(const char *name) : VkMsgWindow(name)
{

  VkSubMenu *pane = addMenuPane("Application");
  pane->addAction("Quit", quitCallback);

  createAction(MESSAGE, 
	       &SampleWindow::messageHandler, 
	       (void*) this,
	       VK_MSG_ACTION_HANDLE);

  updatePatterns();

  XmString received = XmStringCreateLtoR("Received", XmSTRING_DEFAULT_CHARSET);

  _recieveSlider = XtVaCreateManagedWidget("recieve", 
					   xmScaleWidgetClass, 
					   mainWindowWidget(),
					   XmNtitleString,   received,
					   XmNorientation,   XmHORIZONTAL,
					   XmNminimum,       0,
					   XmNmaximum,       100,
					   XmNshowValue,     TRUE,
					   NULL);

  addView(_recieveSlider);
}

SampleWindow::~SampleWindow()
{
  // Empty
}


Boolean SampleWindow::messageHandler(void                *clientData,
				     VkMsgFacilityReason  reason, 
				     VkMsgMessage         msg_in,
				     char                *op, 
				     int                  argc, 
				     VkMsgArg            *argv)
{
  SampleWindow *obj = (SampleWindow *) clientData;

  if (reason == VK_MSG_FACILITY_REQUEST && 
      argc == 2 &&
      VkMsgTypeIsInt(argv[0].type) 
      && !strcmp(op, MESSAGE)) 
    {
      XtVaSetValues(obj->_recieveSlider, XmNvalue, argv[0].value.ival, NULL);

      VkMsgSetIVal(msg_in, 1, (argv[0].value.ival+1) % 100);

      VkMsgReply(msg_in);

      return TRUE;
    }
  else 
    {
      theErrorDialog->post(VkFormat("Unexpected or invalid message format for %s\n", op));

      return FALSE;
    }
}

void SampleWindow::quitCallback(Widget, XtPointer, XtPointer)
{
    theApplication->terminate();
}

void  main(int argc, char **argv)
{
     VkMsgApp *app = new VkMsgApp("Sample2", &argc, argv, NULL, NULL, "SAMPLE2");
     SampleWindow *win = new SampleWindow("service");

     win->show();
     app->run();
}
