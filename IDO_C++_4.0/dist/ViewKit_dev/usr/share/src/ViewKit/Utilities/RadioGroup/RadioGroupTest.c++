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
/////////////////////////////////////////////////////////////
//
// Source file for RadioGroupTest
//
/////////////////////////////////////////////////////////////


#include "RadioGroupTest.h" 
#include <Vk/VkRadioGroup.h>
#include <Xm/BulletinB.h> 
#include <Xm/PushB.h> 
#include <Xm/Text.h> 
#include <Xm/ToggleB.h> 

// These are default resources for widgets in objects of this class
// All resources will be prepended by *<name> at instantiation,
// where <name> is the name of the specific instance, as well as the
// name of the baseWidget. These are only defaults, and may be overriden
// in a resource file by providing a more specific resource name

RadioGroupTest::RadioGroupTest(const char *name, Widget parent) : 
         VkComponent(name) 
{ 
    Arg args[6];
    int count;

    // Create an unmanaged widget as the top of the widget hierarchy

    _baseWidget = XtVaCreateWidget ( _name,
                    xmBulletinBoardWidgetClass,
                    parent,
                    XmNresizePolicy, XmRESIZE_GROW, 
                    XmNwidth, 584, 
                    XmNheight, 501, 
                    NULL ) ;

    // install a callback to guard against unexpected widget destruction

    installDestroyHandler();


    // Create widgets used in this component
    // All variables are data members of this class

    _text = XtVaCreateManagedWidget  ( "text",
                    xmTextWidgetClass,
                    _baseWidget, 
                    XmNx, 30, 
                    XmNy, 90, 
                    XmNwidth, 300, 
                    XmNheight, 285, 
                    NULL ) ;

    _a1 = XtVaCreateManagedWidget  ( "A1",
                    xmToggleButtonWidgetClass,
                    _baseWidget, 
                    XmNx, 20, 
                    XmNy, 20, 
                    NULL ) ;


    _b1 = XtVaCreateManagedWidget  ( "B1",
                    xmToggleButtonWidgetClass,
                    _baseWidget, 
                    XmNx, 270, 
                    XmNy, 20, 
                    NULL ) ;



    _a2 = XtVaCreateManagedWidget  ( "A2",
                    xmToggleButtonWidgetClass,
                    _baseWidget, 
                    XmNx, 380, 
                    XmNy, 110, 
                    NULL ) ;
    _b2 = XtVaCreateManagedWidget  ( "B2",
                    xmToggleButtonWidgetClass,
                    _baseWidget, 
                    XmNx, 380, 
                    XmNy, 220, 
                    NULL ) ;



    _a3 = XtVaCreateManagedWidget  ( "A3",
                    xmToggleButtonWidgetClass,
                    _baseWidget, 
                    XmNx, 270, 
                    XmNy, 430, 
                    NULL ) ;


    _b3 = XtVaCreateManagedWidget  ( "B3",
                    xmToggleButtonWidgetClass,
                    _baseWidget, 
                    XmNx, 30, 
                    XmNy, 430, 
                    NULL ) ;



    _rg1 = new VkRadioGroup();
    _rg2 = new VkRadioGroup();

    _rg1->add(_a1);
    _rg1->add(_a2);
    _rg1->add(_a3);

    _rg2->add(_b1);
    _rg2->add(_b2);
    _rg2->add(_b3);
}


RadioGroupTest::~RadioGroupTest() 
{
    delete _rg1;
    delete _rg2;
}

const char* RadioGroupTest::className() 
{
    return ("RadioGroupTest");
}





