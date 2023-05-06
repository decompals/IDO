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
// sample1.c++: demonstrate basic tooltalk message sending
//
// To see the demo, run the program, select a value with the "send"
// slider, and push the browdcast button. The recieve slider should
// move to the value indicated on the send slider.
//
// Now run as many instances of sample1 as you wish. Broadcasting
// a value from any instance moves the recieve sliders of all 
// other instances.
//
// Note that this example sends a "notice" which does not expect a reply
////////////////////////////////////////////////////////////////////

#include <Vk/VkMsgFacility.h>
#include <Vk/VkMsgWindow.h>
#include <Vk/VkMsgApp.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include <iostream.h>

#define MESSAGE "sample1_value"

// Define a window to hold the interface

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
    // Register a static member function to be called when the
    // given MESSAGE is recieved.
  
    addAction(MESSAGE, SampleWindow::messageHandler, (void *) this,
  	      VK_MSG_ACTION_OBSERVE);


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


Boolean SampleWindow::messageHandler(void                *clientData, 
				     VkMsgFacilityReason,
				     VkMsgMessage,
				     char                *op, 
				     int                 argc, 
				     VkMsgArg            *argv)
{
    // This function is called when a tooltalk message is recieved.
    // Retrieve the object pointer from the client data, check the
    // type of the message and set the recieve slider to
    // the recieved value.

    SampleWindow *obj = (SampleWindow *) clientData;

    if (argc == 1 && VkMsgTypeIsInt(argv[0].type) &&
	!strcmp(op, MESSAGE)) 
    {
	XtVaSetValues(obj->_recieveSlider, XmNvalue, argv[0].value.ival, NULL);
	return True;
    } 
    else 
    {
	cerr << "Unexpected or invalid message format for " << op << "\n" << flush;
	return False;
    }
}

void SampleWindow::broadcastValueCallback(Widget, XtPointer clientData, XtPointer)
{
    SampleWindow *obj = (SampleWindow *) clientData;

    obj->broadcastValue();
}

void SampleWindow::broadcastValue()
{
    int sliderVal;

    // Retrieve the value of the send slider and send a tooltalk 
    // message to broadcast the value.

    XtVaGetValues(_sendSlider, XmNvalue, &sliderVal, NULL);

    messageClient()->sendIntNotice(MESSAGE, sliderVal, 0);
}

void main(int argc, char **argv)
{
    VkMsgApp     *app = new VkMsgApp("Sample1", &argc, argv);
    SampleWindow *win = new SampleWindow("sample1");

    win->show();
    app->run();
}
