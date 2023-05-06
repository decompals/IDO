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


// Classes for menu items in a VkMenu object.
    
#ifndef VKMENUITEM_H
#define VKMENUITEM_H
    
#include <Vk/VkComponent.h>

class VkComponentList;

#define MAXWIDGETS 25


enum  VkMenuItemType {
    END,                     // Used to mark the end of a static menu structure definition
	ACTION,              // A "normal" menu item - uses a pushbutton gadget
	ACTIONWIDGET,        // Same as an action, but forces a widget to be used
	SUBMENU,             // A cascading submenu
	RADIOSUBMENU,        // Same as above, but forced to act as a radio-style pane
	SEPARATOR,           // A separator gadget
	LABEL,               // A label gadget
	TOGGLE,              // A two-state toggle button gadget
	OPTION,              // An XmOption Menu
	POPUP,               // a popup pane
	BAR,                 // a menu bar
        CONFIRMFIRSTACTION,  // An action that will not be executed unless user confirms first
	OBJECT,              // A user-defined subclass of VkMenuActionObject
	STUB,                // Used internally to support undo of arbirtrary callbacks
	};


class VkMenuItem : public VkComponent {
    
    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;

 public:

    ~VkMenuItem();
    
    virtual void setLabel(const char *);
    void setPosition(int );

    void activate();
    void deactivate();
    int remove();
    void show();
    void show(Widget);
    void hide();

    virtual const char* className();
    virtual Boolean isContainer();
    virtual  VkMenuItemType menuType () = 0;
    Widget baseWidget() const;

 protected:

    int                _position;
    Boolean            _isBuilt;
    int                _sensitive;
    class VkMenu      *_parentMenu;
    char              *_label;

    Boolean         _isHidden;

    VkMenuItem();
    VkMenuItem(const char * );

    static  Widget  _unmanagedWidgets[MAXWIDGETS];
    static  int     _numUnmanagedWidgets;
    static  void    manageAll();

    static VkComponentList *_workProcList;

    virtual void build(Widget);
};


class VkMenuAction : public VkMenuItem {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;

  public:
    
    VkMenuAction(const char *name, XtCallbackProc func = NULL, XtPointer clientData = NULL);
    VkMenuAction(const char *, XtCallbackProc, XtCallbackProc, XtPointer clientData = NULL);
    ~VkMenuAction();

    virtual void undo();

    Boolean hasUndo() { return (_undoCallback != NULL); }
    virtual VkMenuItemType menuType ();
    virtual const char* className();

  protected:

    XtCallbackProc  _undoCallback;
    XtCallbackProc  _func;       
    void           *_data;           

    virtual void build(Widget);
};

class VkMenuConfirmFirstAction : public VkMenuAction {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;

  public:
    
    VkMenuConfirmFirstAction(const char *name, XtCallbackProc func = NULL, XtPointer clientData = NULL);
    ~VkMenuConfirmFirstAction();
    virtual VkMenuItemType menuType ();
    virtual const char* className();

  protected:

    virtual void build(Widget);


  private:

     static void actionCallback(Widget, XtPointer, XtPointer);
    
};


class VkMenuActionObject : public VkMenuAction {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;


  public:
    
    VkMenuActionObject(const char *, void *clientData = NULL);
    ~VkMenuActionObject();

    virtual VkMenuItemType menuType ();
    virtual const char* className();

  protected:

    virtual void undoit(void *) = 0;
    virtual void doit(void *) = 0;
    void *_clientData;

  private:

    static void undoCallback(Widget, XtPointer, XtPointer);
    static void doitCallback(Widget, XtPointer, XtPointer);
    
};


class VkMenuActionStub : public VkMenuAction {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;


  public:
    
    VkMenuActionStub(const char *name, XtCallbackProc func = NULL, XtPointer clientData = NULL);
    ~VkMenuActionStub();

    virtual VkMenuItemType menuType ();
    virtual const char* className();

  protected:

     virtual void build(Widget);
};



class VkMenuActionWidget : public VkMenuAction {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;

  public:
    
    VkMenuActionWidget(const char *name, 
		       XtCallbackProc func = NULL, 
		       XtPointer clientData = NULL);

    VkMenuActionWidget(const char *, 
		       XtCallbackProc, 
		       XtCallbackProc func, 
		       XtPointer clientData = NULL);

    ~VkMenuActionWidget();
    virtual  VkMenuItemType menuType ();
    virtual const char* className();

  private:

    virtual void build(Widget);
};




class VkMenuLabel : public VkMenuItem {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;

  public:
    
    VkMenuLabel(const char *);
    ~VkMenuLabel();
    virtual  VkMenuItemType menuType ();
    virtual const char* className();

  protected:

    virtual void build(Widget);
};


class VkMenuSeparator : public VkMenuLabel {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;

  public:
    
    VkMenuSeparator();
    ~VkMenuSeparator();
    virtual VkMenuItemType menuType ();
    virtual const char* className();

  protected:

    virtual void build(Widget);
    
};


class VkMenuToggle : public VkMenuAction {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;

  public:

    VkMenuToggle(const char *, 
		 XtCallbackProc func = NULL, 
		 XtPointer clientData = NULL);

    VkMenuToggle(const char *, 
		 XtCallbackProc, XtCallbackProc, 
		 XtPointer clientData = NULL);

    ~VkMenuToggle();

    virtual  VkMenuItemType menuType ();
    virtual const char* className();
    void setVisualState(Boolean state);
    void setStateAndNotify(Boolean);
    Boolean getState();

  protected:

    int             _state;
    virtual void build(Widget);
};

///////////////////////////////////////////////////////////////////
// Undo support
//
///////////////////////////////////////////////////////////////////

class VkComponentList;
class VkAction;

class VkMenuUndoManager : public VkMenuAction {

    friend  class VkMenu;
    friend  class VkMenuBar;
    friend  class VkPopupMenu;
    friend  class VkOptionMenu;
    friend  class VkSubMenu;

  public:

    VkMenuUndoManager(const char *);
    ~VkMenuUndoManager();

     void  reset();
     void  add(VkMenuAction *);    
     void  add(const char *name, XtCallbackProc undoCallback, XtPointer clientData);
     void  multiLevel (Boolean flag) { _multiLevel = flag; }
     VkComponentList *historyList() { return _commands; }
     virtual const char* className();

  protected:

     VkComponentList *_commands;
     virtual void undo();
     void setUndoLabel();
     virtual void build(Widget);

 
  private:
    
    static void undoCallback(Widget, XtPointer, XtPointer);
    XmString _labelString;
    Boolean  _multiLevel;
    Boolean  _undoMode;

    
};

extern VkMenuUndoManager *getTheUndoManager();
    
#define theUndoManager getTheUndoManager()

#endif












