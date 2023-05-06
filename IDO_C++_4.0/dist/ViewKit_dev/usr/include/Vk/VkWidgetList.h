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
#ifndef VKWidgetList_H
#define VKWidgetList_H

#include <Xm/Xm.h>

class VkComponent;

class VkWidgetList{
    
  public:
    
     VkWidgetList ( );
     virtual ~VkWidgetList ( );
     virtual void add(Widget);
     virtual void add(VkComponent *);
     virtual void remove(VkComponent *);
     virtual void remove(Widget);
     virtual void removeFirst();
     virtual void removeLast();
     virtual Boolean exists (Widget);
     int size() { return _numWidgets; }
     Widget   operator[]  (int index)	const;

  protected:

    virtual void widgetDestroyed ( Widget, XtPointer );

  private:
    
    static void widgetDestroyedCallback ( Widget, XtPointer, XtPointer );
    
    WidgetList _widgets;
    int        _numWidgets;
};
#endif

