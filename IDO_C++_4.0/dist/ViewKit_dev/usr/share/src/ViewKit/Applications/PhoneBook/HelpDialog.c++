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
//
// The help window.  We don't actually display any useful help, but this
// demonstrates how to link a window to the help system.
//

#include <stdio.h>
#include "HelpDialog.h"
#include <Xm/Text.h>

static char *helpText = "\
Phone Book is a sample ViewKit application.\n\
\n\
Copyright 1993, Silicon Graphics, Inc.  All Rights Reserved.\n\
\n\
";

HelpDialog::HelpDialog(const char *docName) : VkSimpleWindow(docName)
{
    Widget scroll, hs;

    _text = XmCreateScrolledText(mainWindowWidget(), "helpText", NULL, 0);

    displayHelp();

    XtManageChild(_text);

    scroll = XtParent(_text);

    XtVaGetValues(scroll, XmNhorizontalScrollBar, &hs, NULL);  
    
    if (hs)
	XtUnmanageChild(hs);
    
    addView(_text);    
}

HelpDialog::~HelpDialog()
{
    // Empty
}

/**********************************************************************/

const char *HelpDialog::className()
{
    return "HelpDialog";
}

/**********************************************************************/

// When the user uses the window manager's Quit item, hide the window
// instead of destroy it.

void HelpDialog::handleWmDeleteMessage()
{
    hide();
}

void HelpDialog::displayHelp()
{
    XmTextSetString(_text, helpText);
}
