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
// This component encapsulates the entrire display area in the main
// window.  It also controls the user's interaction in this area.
//
// A VkTabPanel displays the lettered tabs and the Entries object
// displays a list of database entries.
//

#include <stdio.h>
#include "Info.h"
#include "Entries.h"
#include <Vk/VkTabPanel.h>
#include <Vk/VkApp.h>

#include <Xm/Form.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/DropSMgr.h>

static int count;
static Arg args[10];

Info::Info(const char *name, Widget parent) : VkComponent(name)
{
    Widget cw;

    count = 0;
    _baseWidget = XmCreateForm(parent, (char *) name, args, count);

    // Create the tab panel to show the lettered tabs
    
    panel = new VkTabPanel("panel", _baseWidget);
    count = 0;
    XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    XtSetValues(panel->baseWidget(), args, count);
    panel->show();
    VkAddCallbackMethod(VkTabPanel::tabSelectCallback, panel, this,
			Info::tabSelect, NULL);

    // Create a scrolled window for the entries area, since entries
    // may not fit within the vertical dimension.  Get rid of the
    
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNbottomWidget, panel->baseWidget());  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    scroll = XmCreateScrolledWindow(_baseWidget, "scroll", args, count);
    XtManageChild(scroll);

    vsb = NULL;
    count = 0;
    XtSetArg(args[count], XmNverticalScrollBar, &vsb);  count++;
    XtGetValues(scroll, args, count);

    // Create a blank area that we use to hide the Entries component
    // during transitions
    
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNbottomWidget, panel->baseWidget());  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    blank = XmCreateForm(_baseWidget, "blank", args, count);
    XtManageChild(blank);

    // Create the Entries component for actual entry display
    
    entries = new Entries("entries", scroll, this);
    count = 0;
    XtSetArg(args[count], XmNbackground, panel->tabBg());  count++;
    XtSetValues(entries->baseWidget(), args, count);
    entries->show();
    
    // Get the scrolled window's clip window.  Watch for resizes of
    // the clip window so we can keep the width of the
    // Entries component the same as the clip window's width.
    
    cw = NULL;
    count = 0;
    XtSetArg(args[count], XmNclipWindow, &cw);  count++;
    XtGetValues(scroll, args, count);

    if (cw)
    {
	count = 0;
	XtSetArg(args[count], XmNbackground, panel->tabBg());  count++;
	XtSetValues(cw, args, count);
	XtAddCallback(cw, XmNresizeCallback, &Info::resizeCallback, (XtPointer) this);
    }

    // Add our lettered tabs
    
    panel->addTab("ABC", NULL);
    panel->addTab("DEF", NULL);
    panel->addTab("GHI", NULL);
    panel->addTab("JKL", NULL);
    panel->addTab("MNOP", NULL);
    panel->addTab("QRS", NULL);
    panel->addTab("TUV", NULL);
    panel->addTab("WXYZ", NULL);
}

Info::~Info()
{
    delete panel;
    delete entries;
}

const char *Info::className()
{
    return "Info";
}

// Select a particular lettered range.  Before telling the Entries
// component to redisplay, raise the blank widget to hide any
// visual transition effects.  Reset the scrollbar to the beginning
// if necessary.

void Info::selectRange(char *range, Boolean resetScroll)
{
    int v, s, i, p;
    
    startBlank();
    XmDropSiteStartUpdate(_baseWidget);
    entries->display(range);
    if (resetScroll && vsb && XtIsManaged(vsb))
    {
	XmScrollBarGetValues(vsb, &v, &s, &i, &p);
	if (v != 0)
	{
	    XmScrollBarSetValues(vsb, 0, s, i, p, True);
	}
    }
    XmDropSiteEndUpdate(_baseWidget);
    endBlank();
}

/**********************************************************************/

// The user has selected a tab.  Select the range that matches the
// tab's label.

void Info::tabSelect(VkComponent *, void *, void *callData)
{
    VkTabCallbackStruct *cb = (VkTabCallbackStruct *) callData;
    
    selectRange(cb->label);
}

// When the clip window resizes, change the Entries component's width
// to match.  Leave some spacing.

void Info::resizeCW(Widget cw)
{
    Dimension widthCW, widthEntries;

#define SPACING 2

    count = 0;
    XtSetArg(args[count], XmNwidth, &widthCW);  count++;
    XtGetValues(cw, args, count);
    count = 0;
    XtSetArg(args[count], XmNwidth, &widthEntries);  count++;
    XtGetValues(entries->baseWidget(), args, count);
    if (widthCW-SPACING != widthEntries)
    {
	count = 0;
	XtSetArg(args[count], XmNwidth, widthCW-SPACING);  count++;
	XtSetValues(entries->baseWidget(), args, count);
    }
}

// Raise the blank widget to hide the Entries component and turn on
// the busy cursor.

void Info::startBlank()
{
    theApplication->busy();

    if (XtWindow(blank))
    {
	XRaiseWindow(XtDisplay(blank), XtWindow(blank));
	XFlush(XtDisplay(blank));
    }
}

// Install a work proc to lower the blank widget and turn off the
// busy cursor.  We don't actually do this now, to give Xt a chance
// to settle down.

void Info::endBlank()
{
    XtAppAddWorkProc(XtWidgetToApplicationContext(blank),
		     &Info::blankCallback, (XtPointer) this);
}

// Really lower the blank widget and turn off the busy cursor, when
// Xt is finished doing everything.

void Info::reallyEndBlank()
{
    if (XtWindow(blank))
    {
	XLowerWindow(XtDisplay(blank), XtWindow(blank));
	XFlush(XtDisplay(blank));
    }
    theApplication->notBusy();
}

/**********************************************************************/

void Info::resizeCallback(Widget w, XtPointer clientData, XtPointer)
{
    Info *obj = (Info *) clientData;
    
    obj->resizeCW(w);
}

Boolean Info::blankCallback(XtPointer clientData)
{
    Info *obj = (Info *) clientData;
    
    obj->reallyEndBlank();
    return True;
}
