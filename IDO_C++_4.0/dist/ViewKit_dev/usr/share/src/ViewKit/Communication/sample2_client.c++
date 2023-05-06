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
// sample2_client.c++: demonstrate round-trip tooltalk requests
//
// To see the demo, run the program, select a value with the "send"
// slider, and push the browdcast button. The program sends a request
// and tooltalk attempts to start up a process named sample2_service.
// When that program starts, it replies to the request and sample2_client
// updates the recieved slider. The sample2_service increments the
// value as proof that the messages were processed as expected, so the
// value displayed by the recieved slider will be one larger than the 
// value of the send slider.
//
// NOTE: you must build sample2_service before running this program
//
// NOTE: this program displays a dialog when the tooltalk request
//       is initiated. Clearly this is not the thing to do in a real 
//       program. The dialog is used here for demonstration purposes.
////////////////////////////////////////////////////////////////////

#include <Vk/VkMsgFacility.h>
#include <Vk/VkMsgWindow.h>
#include <Vk/VkMsgApp.h>
#include <Vk/VkErrorDialog.h>
#include <Vk/VkInfoDialog.h>
#include <Vk/VkFormat.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>


#define MESSAGE "sample2_value"

class SampleWindow : public VkMsgWindow {

  public:

     SampleWindow(const char *name);
     ~SampleWindow();

  protected:

     static Boolean messageHandler(void                 *obj,
				   VkMsgFacilityReason   reason,
				   VkMsgMessage          msg_in,
				   char                 *op, 
				   int                   argc, 
				   VkMsgArg             *argv);
    void broadcastValue();
    
    static void broadcastValueCallback(Widget, XtPointer, XtPointer );

    Widget  _sendSlider, _recieveSlider;
};


SampleWindow::SampleWindow(const char *name) : VkMsgWindow(name)
{
    addAction(MESSAGE, SampleWindow::messageHandler, this,
  	      VK_MSG_ACTION_REPLY);

    addAction(MESSAGE, SampleWindow::messageHandler, this,
	      VK_MSG_ACTION_START);

    updatePatterns();

    // Create the UI

    Widget frame = XmCreateForm(mainWindowWidget(), "frame", NULL, 0);


    Widget controls = XtVaCreateManagedWidget("controls", xmRowColumnWidgetClass, frame,
					      XmNtopAttachment,    XmATTACH_FORM,   
					      XmNbottomAttachment, XmATTACH_FORM,
					      XmNrightAttachment,  XmATTACH_FORM,
					      XmNleftAttachment,   XmATTACH_FORM,
					      NULL);

    XmString send = XmStringCreateLtoR("Send", XmSTRING_DEFAULT_CHARSET);

    _sendSlider = XtVaCreateManagedWidget("send", xmScaleWidgetClass, controls,
				     XmNtitleString, send,
				     XmNorientation, XmHORIZONTAL,
				     XmNminimum, 0,
				     XmNmaximum, 100,
				     XmNshowValue, TRUE,
				     NULL);
    XmStringFree(send);

    XmString received = XmStringCreateLtoR("Received", XmSTRING_DEFAULT_CHARSET);

    _recieveSlider = XtVaCreateManagedWidget("recieve", xmScaleWidgetClass, controls,
				    XmNtitleString, received,
				    XmNorientation, XmHORIZONTAL,
				    XmNminimum, 0,
				    XmNmaximum, 100,
				    XmNshowValue, TRUE,
				    NULL);
  
    Widget button = XmCreatePushButton(controls, "Broadcast", NULL, 0);

    XtAddCallback(button, XmNactivateCallback,
		  &SampleWindow::broadcastValueCallback,
		  (XtPointer) this);
    XtManageChild(button);

    addView(frame);
}

SampleWindow::~SampleWindow()
{
  // Empty
}


Boolean SampleWindow::messageHandler(void *client_data,
				     VkMsgFacilityReason reason, 
				     VkMsgMessage,
				     char *op, 
				     int argc, 
				     VkMsgArg *argv)
{
  SampleWindow *obj = (SampleWindow *) client_data;

  if (reason == VK_MSG_FACILITY_REPLY && argc == 2 &&
      VkMsgTypeIsInt(argv[1].type) && !strcmp(op, MESSAGE)) 
  {
      XtVaSetValues(obj->_recieveSlider, XmNvalue, argv[1].value.ival, NULL);
      return True;
    } 
  else if (reason == VK_MSG_FACILITY_STARTED) 
    {
      theInfoDialog->post("Starting service...");
      return False;
    } 
  else 
    {
      theErrorDialog->post(VkFormat("Unexpected or invalid message format for %d", op));
      return False;
    }
}

void SampleWindow::broadcastValue()
{
  int sliderVal;
  
  XtVaGetValues(_sendSlider, XmNvalue, &sliderVal, NULL);

  messageClient()->composeBegin();
  messageClient()->composeAdd(sliderVal);
  messageClient()->composeAdd(0, VK_MSG_OUT);
  messageClient()->sendRequest(MESSAGE);
}

void SampleWindow::broadcastValueCallback(Widget, XtPointer clientData, XtPointer)
{
    SampleWindow *obj = (SampleWindow *) clientData;
    obj->broadcastValue();
}

void main(int argc, char **argv)
{
  VkMsgApp     *app = new VkMsgApp("Sample2", &argc, argv);
  SampleWindow *win = new SampleWindow("client");

  win->show();
  app->run();
}
