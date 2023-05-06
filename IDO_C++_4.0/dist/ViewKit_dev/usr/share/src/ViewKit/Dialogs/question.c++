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
// Simple example of an application that calls the  QuestionDialog
//////////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkQuestionDialog.h>
#include <stdio.h>
#include <Xm/PushB.h>

class QuestionWindow: public VkSimpleWindow {

  protected:

    static void QuestionCallback(Widget, XtPointer, XtPointer);

  public:

    QuestionWindow ( const char *name );
    ~QuestionWindow ( );
    virtual const char* className();
};

QuestionWindow::~QuestionWindow ( )
{
    // Empty
}

const char* QuestionWindow::className() { return "QuestionWindow"; }

QuestionWindow::QuestionWindow ( const char *name ) : VkSimpleWindow ( name ) 
{
    Widget button =  XmCreatePushButton ( mainWindowWidget(), "Question", NULL, 0 );

    XtAddCallback(button, XmNactivateCallback, 
		  &QuestionWindow::QuestionCallback, 
		  (XtPointer) this);

    addView(button);
}


void QuestionWindow::QuestionCallback(Widget, XtPointer, XtPointer)
{
    if(theQuestionDialog->postAndWait("Yes?"))
	printf("yes\n");
    else
	printf("no\n");
}

void main ( int argc, char **argv )
{
    VkApp       *app = new VkApp("QuestionApp", &argc, argv);
    QuestionWindow  *win1 = new QuestionWindow("Question");
    QuestionWindow  *win2 = new QuestionWindow("Question2");

    win1->show();
    win2->show();

    app->run();
}




