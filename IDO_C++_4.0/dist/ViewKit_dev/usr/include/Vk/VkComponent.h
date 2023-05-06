
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


#ifndef VKCOMPONENT_H
#define VKCOMPONENT_H

#include <Xm/Xm.h>
#include <Vk/VkCallbackObject.h>

class VkComponent : public VkCallbackObject {

 public:

    virtual ~VkComponent();

    virtual void show();      // make the component visible
    virtual void hide();      // remove the component from the screen
    virtual void realize();   // Shouldn't use in normal circumstances

    void manage()   { show(); } // For compatibility with components based on C++/Motif book
    void unmanage() { hide(); } // For compatibility with components based on C++/Motif book
  
    const char * name() const { return _name; }
    virtual const char *className();
    Widget baseWidget() const;

    virtual Boolean okToQuit();

    static Boolean isComponent(VkComponent *);

    static const char * const deleteCallback;

    virtual operator Widget () const;

    virtual void setAttribute(const char *, void *); // For future use

    virtual char **attributeList();                  // For future use

    virtual void getAttribute(const char *, void **); // For future use

  protected:

    // Support for dynamic widget destruction

    void installDestroyHandler();   // Easy hook for derived classes
    void removeDestroyHandler();    // Easy hook for derived classes
    virtual void widgetDestroyed(); // Called when base widget is destroyed

    // Suport for doing things after realize time

    virtual void afterRealizeHook();

    // Support for using the X resource manager

    void setDefaultResources ( const Widget , const String * );
    void getResources ( const XtResourceList, const int );

    // members useful to derived classes

    char   *_name;
    Widget  _baseWidget;    
    Widget& _w;       // For compatibility with components based on C++/Motif book

    VkComponent( const char *name );     // Protected constructor forces subclasses to redefine
    VkComponent();   // Default constructor should never be used, but may be called from subclass
                     // default constructors, which should probably also not be used....

    void *_extension;
    
  private:

   // Support for sanity check of object validity

    VkComponent *_self;  

    static void widgetDestroyedCallback ( Widget, 
					  XtPointer, 
					  XtPointer );

    static void afterRealizeEventHandler ( Widget, 
					   XtPointer, 
					   XEvent *,
					   Boolean *);

    
};


// A convenience for applications that need to pass both an object and other client data
// This should almost NEVER be needed

typedef struct {
    void *client_data;
    void *obj;
} VkCallbackStruct;

#endif
