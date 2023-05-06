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

#ifndef VKAPP_H
#define VKAPP_H

//////////////////////////////////////////////////////////////////////////////////////////
// VkApp.h

#include <Vk/VkComponent.h>
#include <Vk/VkComponentList.h>

class VkApp;
class VkSimpleWindow;
class VkBusyDialog;
class VkDialogManager;
class VkCursorList;

extern VkApp             *theApplication;
extern unsigned int       VkDebugLevel;

// VkApp class

class VkApp : public VkComponent {

    friend VkSimpleWindow;

  public:

    VkApp(char             *appClassName,
	  int              *arg_c, 
	  char            **arg_v,
	  XrmOptionDescRec *optionList       = NULL,
	  int               sizeOfOptionList = 0);

    VkApp(Widget w);  // Test

     static const int ViewKitMajorRelease;
     static const int ViewKitMinorRelease;
     static const char ViewKitReleaseString[];

     void   setVersionString(const char *str);
     const char  *versionString() { return _versionString;}

     virtual      ~VkApp();

    virtual void run();
    virtual void terminate(int status = 0);
    virtual void handlePendingEvents();
    virtual void quitYourself();
    virtual void handleRawEvent(XEvent *event);

  
    void         setMainWindow(VkSimpleWindow *);  // Let the app know that a particular "window" is the main one

    // Operations on all windows

    virtual void raise();
    virtual void lower();
    virtual void iconify();
    virtual void open();
    virtual void show();
    virtual void hide();

    void    startupIconified(const Boolean flag) { _startupIconified = flag; }

    virtual Cursor  busyCursor();
    virtual Cursor  normalCursor();
    void    setNormalCursor(const Cursor);
    void    setBusyCursor(const Cursor);
    void    setBusyCursor(VkCursorList *);
    void    showCursor(const Cursor);
    void    setAboutDialog(VkDialogManager *d) { _aboutDialog = d; }
    VkDialogManager*  aboutDialog() { return (_aboutDialog); }

    void    setStartupDialog(VkDialogManager *d) { _startupDialog = d; }
    VkDialogManager*  startupDialog() { return (_startupDialog); }

    virtual void         busy(const char *msg = NULL,     VkSimpleWindow *parent = NULL);
    virtual void         veryBusy(const char *msg = NULL, VkSimpleWindow *parent = NULL);
    virtual void         notBusy();

    virtual void         progressing(const char *msg = NULL);

    void  setBusyDialog(VkBusyDialog *);

    XtAppContext        appContext() const { return _appContext;}
    char               *name() const;
    Display            *display() const { return _dpy;}
    char              **argv() const { return _argv; }
    char               *argv(int index);
    int                 argc() const { return _argc;}
    char               *applicationClassName() const { return _applicationClassName;}
    VkSimpleWindow     *mainWindow() const;
    char               *shellGeometry() const;  // The size of the hidden shell
    Boolean             startupIconified() const { return _startupIconified; }
    Boolean             isBusy() { return (_busyCounter > 0); }
    virtual const char *className();

  protected:

    VkComponentList    _winList;
    int parseCommandLine(XrmOptionDescRec  *, Cardinal); 
    virtual void afterRealizeHook();
    Boolean _quitSemaphore;

    VkCursorList  *_busyCursorList;

  private:

    static  XtResource _resSpec[];
    static  String _resources[];

    static Cursor _busyCursor;
    static Cursor _normalCursor;

    VkDialogManager *_aboutDialog;
    VkDialogManager *_startupDialog;

    void        createCursors();

    // Various data needed by other parts of an application


    char              **_argv;
    int                 _argc;
    char               *_shellGeometry;
    VkSimpleWindow     *_mainWindow;
    char               *_versionString;
    Display            *_dpy;
    XtAppContext        _appContext;
    char               *_applicationClassName;
    void                 addWindow ( VkSimpleWindow * );
    void                 removeWindow( VkSimpleWindow * );
    int                 _busyCounter;
    Boolean             _startupIconified;
    Boolean             _usePopupPlanes;

    
}; 

#endif







