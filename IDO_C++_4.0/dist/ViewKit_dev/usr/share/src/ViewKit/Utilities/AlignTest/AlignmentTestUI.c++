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
// Source file for AlignmentTestUI
//

/////////////////////////////////////////////////////////////


#include "AlignmentTestUI.h"  // Generated header file for this class
#include <Xm/BulletinB.h> 
#include <Xm/PushB.h> 



// These are default resources for widgets in objects of this class
// All resources will be prepended by *<name> at instantiation,
// where <name> is the name of the specific instance, as well as the
// name of the baseWidget. These are only defaults, and may be overriden
// in a resource file by providing a more specific resource name

String  AlignmentTestUI::_defaultAlignmentTestUIResources[] = {
        NULL
};

AlignmentTestUI::AlignmentTestUI(const char *name, Widget parent) : 
         VkComponent(name) 
{ 
    Arg args[3];
    int count;

    // Load any class-defaulted resources for this object

    setDefaultResources(parent, _defaultAlignmentTestUIResources  );


    // Create an unmanaged widget as the top of the widget hierarchy

    _baseWidget = XtVaCreateWidget ( _name,
                    xmBulletinBoardWidgetClass,
                    parent,
                    XmNresizePolicy, XmRESIZE_GROW, 
                    XmNwidth, 638, 
                    XmNheight, 455, 
                    NULL ) ;

    // install a callback to guard against unexpected widget destruction

    installDestroyHandler();


    // Create widgets used in this component
    // All variables are data members of this class

    _pushButton = XtVaCreateManagedWidget  ( "pushButton",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 520, 
                    XmNy, 280, 
                    NULL ) ;


    _pushButton1 = XtVaCreateManagedWidget  ( "pushButton1",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 400, 
                    XmNy, 340, 
                    NULL ) ;


    _pushButton2 = XtVaCreateManagedWidget  ( "pushButton2",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 240, 
                    XmNy, 320, 
                    NULL ) ;


    _pushButton3 = XtVaCreateManagedWidget  ( "pushButton3",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 40, 
                    XmNy, 340, 
                    NULL ) ;


    _qwerty = XtVaCreateManagedWidget  ( "qwerty",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 240, 
                    XmNy, 220, 
                    NULL ) ;


    _a = XtVaCreateManagedWidget  ( "a",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 340, 
                    XmNy, 160, 
                    NULL ) ;


    _aa = XtVaCreateManagedWidget  ( "aa",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 240, 
                    XmNy, 100, 
                    NULL ) ;


    _alalalalalalalalalala = XtVaCreateManagedWidget  ( "alalalalalalalalalala",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 240, 
                    XmNy, 20, 
                    NULL ) ;


    _veryverylong = XtVaCreateManagedWidget  ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaveryverylong",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 20, 
                    XmNy, 200, 
                    NULL ) ;


    _longer = XtVaCreateManagedWidget  ( "longer",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 20, 
                    XmNy, 140, 
                    NULL ) ;


    _long = XtVaCreateManagedWidget  ( "long",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 20, 
                    XmNy, 80, 
                    NULL ) ;


    _short = XtVaCreateManagedWidget  ( "short",
                    xmPushButtonWidgetClass,
                    _baseWidget, 
                    XmNrecomputeSize, TRUE, 
                    XmNx, 20, 
                    XmNy, 40, 
                    NULL ) ;


}


AlignmentTestUI::~AlignmentTestUI() 
{
    // Empty Destructor. Base class destroys widgets
}

const char* AlignmentTestUI::className() 
{
    return ("AlignmentTestUI");
}





