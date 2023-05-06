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

/////////////////////////////////////////////////////////////////
// Demo how to post a dialog
////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkWarningDialog.h>
#include <Vk/VkQuestionDialog.h>
#include <Xm/PushB.h>
#include <stdio.h>

class MyWindow: public VkSimpleWindow {

  protected:

    static void postCallback(Widget, XtPointer, XtPointer);

  public:

    MyWindow ( const char *name );
    ~MyWindow ( );
    virtual const char* className();
};

MyWindow::MyWindow ( const char *name ) : VkSimpleWindow ( name ) 
{
    Widget button =  XmCreatePushButton ( mainWindowWidget(), "Push Me", NULL, 0 );

    XtAddCallback(button, XmNactivateCallback, 
		  &MyWindow::postCallback, 
		  (XtPointer) this);
    addView(button);
}


const char* MyWindow::className() { return "MyWindow"; }

MyWindow::~MyWindow()
{
    // Empty
}


char buf[] = "Warning\n\
 Host down or no object server running on perseus.corp.sgi.com\n\
 CAdmin: No such name on estrogen.corp.sgi.com\n\
 CAdmin: No such name on boingo.wpd.sgi.com\n\
 Host down or no object server running on piju.csd.sgi.com\n\
 Host down or no object server running on sharon.csd.sgi.com\n\
 CAdmin: No such name on suma.asd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on decibel.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on mondigo.esd.sgi.com\n\
 Host down or no object server running on trident.wpd.sgi.com\n\
 Host down or no object server running on sponge.csd.sgi.com\n\
 Host down or no object server running on stabler.esd.sgi.com\n\
 Host down or no object server running on tour2.corp.sgi.com\n\
 Host down or no object server running on phantom.corp.sgi.com\n\
 Host down or no object server running on toast.esd.sgi.com\n\
 Host down or no object server running on joker.asd.sgi.com\n\
 Host down or no object server running on pauper.corp.sgi.com\n\
 Host down or no object server running on einstein.csd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on miranda.asd.sgi.com\n\
 CAdmin: No such name on skyline.esd.sgi.com\n\
 Host down or no object server running on kisio.esd.sgi.com\n\
 Host down or no object server running on moxie.esd.sgi.com\n\
 Host down or no object server running on mollyfox.csd.sgi.com\n\
 Host down or no object server running on shangrila.wpd.sgi.com\n\
 Host down or no object server running on tracy.wpd.sgi.com\n\
 Host down or no object server running on puff.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on snowy.csd.sgi.com\n\
 Host down or no object server running on klunker.mfg.sgi.com\n\
 Host down or no object server running on serf15.esd.sgi.com\n\
 Host down or no object server running on dsstest1.esd.sgi.com\n\
 Host down or no object server running on mach3.esd.sgi.com\n\
 Host down or no object server running on whip.mti.sgi.com\n\
 Host down or no object server running on fh8.esd.sgi.com\n\
 Host down or no object server running on mountain.wpd.sgi.com\n\
 Host down or no object server running on chaz.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on dream.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on prophet.esd.sgi.com\n\
 Host down or no object server running on scsi2.esd.sgi.com\n\
 Host down or no object server running on hoshi.corp.sgi.com\n\
 Host down or no object server running on lavache.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on ps-legal.corp.sgi.com\n\
 Host down or no object server running on mycool.asd.sgi.com\n\
 Host down or no object server running on manny.esd.sgi.com\n\
 Host down or no object server running on psycho.wpd.sgi.com\n\
 Host down or no object server running on swl_mag4.esd.sgi.com\n\
 Host down or no object server running on squoze.esd.sgi.com\n\
 Host down or no object server running on rigel.asd.sgi.com\n\
 Host down or no object server running on crusty.asd.sgi.com\n\
 Host down or no object server running on crusty.asd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on perseus.corp.sgi.com\n\
 CAdmin: No such name on estrogen.corp.sgi.com\n\
 CAdmin: No such name on boingo.wpd.sgi.com\n\
 Host down or no object server running on piju.csd.sgi.com\n\
 Host down or no object server running on sharon.csd.sgi.com\n\
 CAdmin: No such name on suma.asd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on decibel.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on mondigo.esd.sgi.com\n\
 Host down or no object server running on trident.wpd.sgi.com\n\
 Host down or no object server running on sponge.csd.sgi.com\n\
 Host down or no object server running on stabler.esd.sgi.com\n\
 Host down or no object server running on tour2.corp.sgi.com\n\
 Host down or no object server running on phantom.corp.sgi.com\n\
 Host down or no object server running on toast.esd.sgi.com\n\
 Host down or no object server running on joker.asd.sgi.com\n\
 Host down or no object server running on pauper.corp.sgi.com\n\
 Host down or no object server running on einstein.csd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on miranda.asd.sgi.com\n\
 CAdmin: No such name on skyline.esd.sgi.com\n\
 Host down or no object server running on kisio.esd.sgi.com\n\
 Host down or no object server running on moxie.esd.sgi.com\n\
 Host down or no object server running on mollyfox.csd.sgi.com\n\
 Host down or no object server running on shangrila.wpd.sgi.com\n\
 Host down or no object server running on tracy.wpd.sgi.com\n\
 Host down or no object server running on puff.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on snowy.csd.sgi.com\n\
 Host down or no object server running on klunker.mfg.sgi.com\n\
 Host down or no object server running on serf15.esd.sgi.com\n\
 Host down or no object server running on dsstest1.esd.sgi.com\n\
 Host down or no object server running on mach3.esd.sgi.com\n\
 Host down or no object server running on whip.mti.sgi.com\n\
 Host down or no object server running on fh8.esd.sgi.com\n\
 Host down or no object server running on mountain.wpd.sgi.com\n\
 Host down or no object server running on chaz.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on dream.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on prophet.esd.sgi.com\n\
 Host down or no object server running on scsi2.esd.sgi.com\n\
 Host down or no object server running on hoshi.corp.sgi.com\n\
 Host down or no object server running on lavache.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on ps-legal.corp.sgi.com\n\
 Host down or no object server running on mycool.asd.sgi.com\n\
 Host down or no object server running on manny.esd.sgi.com\n\
 Host down or no object server running on psycho.wpd.sgi.com\n\
 Host down or no object server running on swl_mag4.esd.sgi.com\n\
 Host down or no object server running on squoze.esd.sgi.com\n\
 Host down or no object server running on rigel.asd.sgi.com\n\
 Host down or no object server running on crusty.asd.sgi.com\n\
 Host down or no object server running on crusty.asd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
 Host down or no object server running on legal.corp.sgi.com\n\
 Host down or no object server running on mitislow.esd.sgi.com\n\
 Host down or no object server running on (unknown host)\n\
";

void MyWindow::postCallback(Widget, XtPointer clientData, XtPointer)
{
    theQuestionDialog->post(buf);
}


void main ( int argc, char **argv )
{
    VkApp     *app  = new VkApp("Dialog", &argc, argv);
    MyWindow  *win  = new MyWindow("Dialog");
    MyWindow  *win2 = new MyWindow("Dialog2");

    win->show();
    win2->show();
    app->run();
}

