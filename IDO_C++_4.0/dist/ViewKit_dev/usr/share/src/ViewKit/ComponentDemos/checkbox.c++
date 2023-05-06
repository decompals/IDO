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
// checkbox.c++: Test of the VkCheckBox component, a simpe
// way to create multi-value selections
////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkCheckBox.h>

class CheckBoxWindow: public VkSimpleWindow {

  protected:

    void valueChanged(VkComponent *, void *, void *);

  public:

    CheckBoxWindow ( const char *name );

    ~CheckBoxWindow();

    virtual const char* className();
};

CheckBoxWindow::CheckBoxWindow ( const char *name ) : VkSimpleWindow ( name ) 
{
    VkCheckBox *cb = new VkCheckBox("check", mainWindowWidget());

    cb->addItem("one");
    cb->addItem("two");
    cb->addItem("three");
    cb->addItem("four");

    VkAddCallbackMethod( VkCheckBox::itemChangedCallback, 
			 cb, 
			 this, 
			 &CheckBoxWindow::valueChanged, NULL); 

    addView(cb);
}

CheckBoxWindow::~CheckBoxWindow()
{

}

void CheckBoxWindow::valueChanged(VkComponent* obj, void *, void *callData)
{
    VkCheckBox *cb = (VkCheckBox*) obj;
    int index = (int) callData;

    cout << "item # " 
	 << index 
	 << " changed to " 
	 << cb->getValue(index) 
	 << "\n" << flush;
}

const char* CheckBoxWindow::className() { return "CheckBoxWindow"; }

void main ( int argc, char **argv )
{
    VkApp        *cbApp = new VkApp("checkBoxApp", &argc, argv);
    CheckBoxWindow  *cbWin = new CheckBoxWindow("checkbox");

    cbWin->show();
    cbApp->run();
}





