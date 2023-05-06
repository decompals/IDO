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
#ifndef INFO_H
#define INFO_H

#include <Vk/VkComponent.h>

class VkTabPanel;
class Entries;

class Info : public VkComponent {
  public:
    Info(const char *name, Widget parent);
    ~Info();
    
    const char *className();
    
    void selectRange(char *range, Boolean resetScroll = True);
    Entries *getEntries() { return entries; }
    
  protected:
    void tabSelect(VkComponent *comp, void *clientData, void *callData);
    void resizeCW(Widget cw);
    void startBlank();
    void endBlank();
    void reallyEndBlank();
    
    static void resizeCallback(Widget w, XtPointer clientData, XtPointer callData);
    static Boolean blankCallback(XtPointer clientData);
    
    Widget blank;
    VkTabPanel *panel;
    Entries *entries;
    Widget scroll, vsb;
};

#endif
