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
// Source file for AlignmentTest
//
/////////////////////////////////////////////////////////////


#include "AlignmentTest.h"
#include <Vk/VkAlignmentGroup.h>
#include <Vk/VkOptionMenu.h> 
#include <Vk/VkMenuItem.h> 
#include <Xm/TextF.h> 

void func(Widget, XtPointer, XtPointer)
{

}
AlignmentTest::AlignmentTest(const char *name, Widget parent) : 
         AlignmentTestUI(name, parent) 
{ 


    VkOptionMenu *menu = new VkOptionMenu();
    menu->addAction("one", func);
    menu->addAction("two  nnna ", func);
    menu->addAction("three e      eeee", func);

    menu->build(_baseWidget);

    Widget text = XmCreateTextField(_baseWidget, "text", NULL, 0);
    XtVaSetValues(text, XmNy, 300, NULL);
    XtManageChild(text);

    VkAlignmentGroup *ag1 = new VkAlignmentGroup();

    ag1->add(_pushButton);
    ag1->add(_pushButton1);
    ag1->add(_pushButton2);
    ag1->add(_pushButton3);

    ag1->alignHeight();
    ag1->alignTop();


    VkAlignmentGroup *ag2 = new VkAlignmentGroup();

    ag2->add(_alalalalalalalalalala);
    ag2->add(_aa);
    ag2->add(_a);
    ag2->add(_qwerty);
    ag2->add(menu);
    ag2->alignWidth();
    ag2->alignLeft();


    VkAlignmentGroup *ag3 = new VkAlignmentGroup();

    ag3->add(_short);
    ag3->add(_long);
    ag3->add(_longer);
    ag3->add(_veryverylong);
    ag3->add(text);

    ag3->alignWidth();
    ag3->alignLeft();
}


AlignmentTest::~AlignmentTest()
{
  // Empty
}


const char* AlignmentTest::className() 
{
    return ("AlignmentTest");
}



