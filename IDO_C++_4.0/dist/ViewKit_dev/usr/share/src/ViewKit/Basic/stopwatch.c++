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

///////////////////////////////////////////////////////////////////////////
// stopwatch.c++: A simple stopwatch program using the viewkit
//////////////////////////////////////////////////////////////////////////
#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkPeriodic.h>
#include <Vk/VkFormat.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>

class Stopwatch : public VkSimpleWindow {

  public:
  
    Stopwatch(char*name);
    ~Stopwatch();
	
  protected:

    void start(Widget, XtPointer);
    void stop(Widget, XtPointer);
  
    void setTime(long);
    void updateTime(VkCallbackObject *, void *, void *);

  private:

    Widget _stopwatch;
    Widget _stopButton;
    Widget _startButton;
    Widget _row1;
    Widget _row2;
    Widget _label;
    Widget _face;

    static void startCallback(Widget, XtPointer, XtPointer);
    static void stopCallback(Widget, XtPointer, XtPointer);

    VkPeriodic  *_timer;
    long         _curTime;
};

// The constructor uses several SgGrid widgets for the basic layout,
// creates buttons and labels and installs callbacks to start
// and stop the timer.
	
Stopwatch::Stopwatch(char*name)  : VkSimpleWindow(name)
{
      _stopwatch = XtVaCreateManagedWidget ( "stopwatch", 
					     xmRowColumnWidgetClass, 
					     mainWindowWidget(),
					     XmNnumColumns, 1,
					     XmNorientation, XmVERTICAL,
					     XmNpacking, XmPACK_COLUMN,					     
					     NULL );

      _row1 = XtVaCreateManagedWidget ( "row1", 
					xmRowColumnWidgetClass, 
					_stopwatch,
					XmNnumColumns, 2,
					NULL );

      _row2 = XtVaCreateManagedWidget ( "row2", 
					xmRowColumnWidgetClass, 
					_stopwatch,
					XmNnumColumns, 2,
					NULL );

      _label = XtVaCreateManagedWidget ( "label", 
					 xmLabelWidgetClass, 
					 _row1,
					 NULL);

      _face  = XtVaCreateManagedWidget ( "time", 
					 xmTextFieldWidgetClass, 
					 _row1,
					 XmNcursorPositionVisible, FALSE,
					 NULL);

      _startButton  = XtVaCreateManagedWidget ( "start", 
						xmPushButtonWidgetClass, 
						_row2,
						NULL);						

      _stopButton  = XtVaCreateManagedWidget ( "stop", 
					       xmPushButtonWidgetClass, 
					       _row2,
					       NULL);


      XtAddCallback(_startButton, XmNactivateCallback, 
		    &Stopwatch::startCallback, 
		    (XtPointer) this);
      XtAddCallback(_stopButton, XmNactivateCallback, 
		    Stopwatch::stopCallback, (XtPointer) this);

      setTime(_curTime  = 0);
      
      _timer = new VkPeriodic(100);

      VkAddCallbackMethod(VkPeriodic::timerCallback, 
			  _timer, this, 
			  &Stopwatch::updateTime, NULL);
      
      addView(_stopwatch);
}

Stopwatch::~Stopwatch()
{
    delete _timer;
}

void Stopwatch::startCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    Stopwatch * obj = (Stopwatch*) clientData;

    obj->start(w, callData);
}

void Stopwatch::stopCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    Stopwatch * obj = (Stopwatch*) clientData;

    obj->stop(w, callData);
}

void Stopwatch::start(Widget, XtPointer)
{
    _timer->start();
}
void Stopwatch::stop(Widget, XtPointer)
{
    _timer->stop();
}

void Stopwatch::updateTime(VkCallbackObject *, void *, void *)
{
    // Just increment the time (which is assumed to be seconds).
    // Because we're using Xt timers, this is not really very accurate.

    setTime(++_curTime);
}

void Stopwatch::setTime(long t)
{
    XtVaSetValues(_face, 
		  XmNvalue, VkFormat( "%04.1hf", (float) t / 10.0), 
		  NULL);
}
	
void main(int argc, char** argv) 
{
    VkApp *app = new VkApp("Stopwatch", &argc, argv);
    Stopwatch *win = new Stopwatch("Stopwatch");

    win->show();
    app->run();
}


    
				    
