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


#ifndef VKDIALOGMANAGER_H
#define VKDIALOGMANAGER_H

#include <Vk/VkComponent.h>
#include <Vk/VkWidgetList.h>

class VkSimpleWindow;
class VkDialog;


class VkDialogManager : public VkComponent {

  public:

    enum VkDialogReason { CANCEL, OK, APPLY, NONE };

   protected:

    VkDialogReason _action;
    Boolean _showOK, _showCancel, _showApply;
    Boolean _allowMultipleDialogs;
    Boolean _minimizeMultipleDialogs;

    virtual void setUpWindowManagerProperties(Widget w);

    Widget prepost ( const char *, 
		     const char  *, 		       
		     VkSimpleWindow *);    		     


    virtual Widget prepost ( const char      *message, 
			     XtCallbackProc   okCB       = NULL, 
			     XtCallbackProc   cancelCB   = NULL, 
			     XtCallbackProc   applyCB    = NULL, 
			     XtPointer        clientData = NULL,
			     const char      *helpString = NULL,
			     VkSimpleWindow  *parent     = NULL);

    void waitForDialog(Widget, VkSimpleWindow *, Widget);

    virtual void ok(Widget, XtPointer);
    virtual void cancel(Widget, XtPointer);
    virtual void apply(Widget, XtPointer);

    virtual Widget createDialog(Widget parent) = 0;

    VkDialogManager(const char *);

   public:

    virtual ~VkDialogManager();
    const char * className();

    Widget lastPosted();

    void setTitle(const char *nextTitle = NULL);
    void setButtonLabels(const char *ok = NULL, const char *cancel = NULL, const char * apply = NULL);

    static const char *const prepostCallback;

    void unpost();
    void unpost(Widget w);
    void unpostAll();

    void centerOnScreen(Boolean flag) { _centerMode = flag; }

    virtual void hide();  // Same as unpost
    virtual void show();  // alias for post( /* no args */ )

    
    // Derived class that want to modify post behavior should override only these functions

    virtual Widget post ( const char      *message    = NULL, 
			  XtCallbackProc   okCB       = NULL, 
			  XtCallbackProc   cancelCB   = NULL, 
			  XtCallbackProc   applyCB    = NULL, 
			  XtPointer        clientData = NULL,
			  const char      *helpString = NULL, 		       
 			  VkSimpleWindow  *parent     = NULL,
			  Widget           parentWidget = NULL);


    virtual Widget postModal ( const char      *message    = NULL, 
			       XtCallbackProc   okCB       = NULL, 
			       XtCallbackProc   cancelCB   = NULL, 
			       XtCallbackProc   applyCB    = NULL, 
			       XtPointer        clientData = NULL,
			       const char      *helpString = NULL, 		       
			       VkSimpleWindow  *parent     = NULL,
			       Widget           parentWidget = NULL);			       


    virtual Widget postBlocked ( const char      *message    = NULL, 
				 XtCallbackProc   okCB       = NULL, 
				 XtCallbackProc   cancelCB   = NULL, 
				 XtCallbackProc   applyCB    = NULL, 
				 XtPointer        clientData = NULL,
				 const char      *helpString = NULL, 		       
				 VkSimpleWindow  *parent     = NULL,
				 Widget           parentWidget = NULL);			       				 


    virtual VkDialogReason postAndWait ( const char      *message    = NULL, 
				         int          ok         = -1, 
				         int          cancel     = -1, 
				         int          apply      = -1, 
				         const char      *helpString = NULL, 		       
				         VkSimpleWindow  *parent     = NULL,
					 Widget           parentWidget = NULL);

  // These are conveniences

    Widget post ( const char *msg, 
		  VkSimpleWindow *parent) { return post(msg, NULL, NULL,
						       NULL, NULL, NULL, parent); }

    Widget post ( const char *msg, 
		  Widget parent) {   return post(msg, NULL, NULL,
						 NULL, NULL, NULL, NULL, parent); }    

    Widget post ( const char     *msg, 
		  const char     *helpString, 		       
		  VkSimpleWindow *parent) { return post(msg, NULL,
							NULL, NULL, NULL, helpString, parent); }

    Widget post ( const char     *msg, 
		  const char     *helpString, 		       
		  Widget parent) { return post(msg, NULL, NULL,
					       NULL, NULL, helpString, NULL, parent); }    

    Widget post ( const char      *msg, 
		  XtCallbackProc   okCB, 
		  XtPointer        clientData,
		  VkSimpleWindow  *parent = NULL)  { return post(msg, okCB,
								 NULL, NULL, clientData, NULL, parent); }

    Widget post ( const char      *msg, 
		  XtCallbackProc   okCB, 
		  XtPointer        clientData,
		  Widget           parent)  { return post(msg, okCB, NULL, NULL,
							 clientData, NULL, NULL, parent); }    

    Widget post ( const char      *msg, 
		  XtCallbackProc   okCB, 
		  XtPointer        clientData,
		  const char     *helpString, 		       
		  VkSimpleWindow  *parent = NULL)  { return post(msg, okCB, NULL, NULL,
								 clientData, helpString, parent); }

    Widget post ( const char      *msg, 
		  XtCallbackProc   okCB, 
		  XtPointer        clientData,
		  const char     *helpString, 		       
		  Widget          parent)  { return post(msg, okCB, NULL,
							 NULL, clientData, helpString, NULL, parent); }    


   Widget post ( const char     *msg, 
		 XtCallbackProc  okCB, 
		 XtCallbackProc  cancelCB, 
		 XtPointer       clientData,
		 VkSimpleWindow *parent = NULL)  { return post(msg, okCB, cancelCB,
							       NULL, clientData, NULL, parent); }

    Widget post ( const char     *msg, 
		  XtCallbackProc  okCB, 
		  XtCallbackProc  cancelCB, 
		  XtPointer        clientData,
		  Widget parent)  { return post(msg, okCB, cancelCB,
						NULL, clientData, NULL, NULL, parent); }    

   Widget post ( const char     *msg, 
		 XtCallbackProc  okCB, 
		 XtCallbackProc  cancelCB, 
		 XtPointer       clientData,
		 char            *helpString,
		 VkSimpleWindow *parent = NULL)  { return post(msg, okCB, cancelCB, NULL, clientData, helpString, parent); }

    Widget post ( const char     *msg, 
		  XtCallbackProc  okCB, 
		  XtCallbackProc  cancelCB, 
		  XtPointer       clientData,
		  char            *helpString,
		  Widget          parent)  { return post(msg, okCB, cancelCB, NULL,
							 clientData, helpString, NULL, parent); }    

   Widget post ( const char  *msg, 
	      XtCallbackProc  okCB, 
	      XtCallbackProc  cancelCB, 
	      XtCallbackProc  applyCB, 
	      XtPointer       clientData,
	      VkSimpleWindow *parent)  { return post(msg, okCB, cancelCB,
						     applyCB, clientData, NULL, parent); }

   Widget post ( const char     *msg, 
		 XtCallbackProc  okCB, 
		 XtCallbackProc  cancelCB, 
		 XtCallbackProc  applyCB, 
		 XtPointer       clientData,
		 Widget parent)  { return post(msg, okCB, cancelCB, applyCB, clientData, NULL, NULL, parent); }    


  // Modal

  Widget postModal ( const char *msg, 
		     VkSimpleWindow *parent) { return postModal(msg, NULL, NULL, NULL, NULL, NULL, parent); }
  
  Widget postModal ( const char     *msg, 
		     const char     *helpString, 		       
		     VkSimpleWindow *parent) { return postModal(msg, NULL, NULL, NULL, NULL, helpString, parent); }

  Widget postModal ( const char      *msg, 
		     XtCallbackProc   okCB, 
		     XtPointer        clientData,
		     VkSimpleWindow  *parent = NULL)  { return postModal(msg, okCB, NULL, NULL, clientData, NULL, parent); }

  Widget postModal ( const char      *msg, 
		     XtCallbackProc   okCB, 
		     XtPointer       clientData,
		     const char     *helpString, 		       
		     VkSimpleWindow  *parent = NULL)  { return postModal(msg, okCB, NULL, NULL, clientData, helpString, parent); }


  Widget postModal ( const char     *msg, 
		     XtCallbackProc  okCB, 
		     XtCallbackProc  cancelCB, 
		     XtPointer       clientData,
		     VkSimpleWindow *parent = NULL)  { return postModal(msg, okCB, cancelCB, NULL, clientData, NULL, parent); }

  Widget postModal ( const char     *msg, 
		     XtCallbackProc  okCB, 
		     XtCallbackProc  cancelCB, 
		     XtPointer       clientData,
		     char            *helpString,
		     VkSimpleWindow *parent = NULL)  { return postModal(msg, okCB, cancelCB, NULL, clientData, helpString, parent); }

  Widget postModal ( const char     *msg, 
		     XtCallbackProc  okCB, 
		     XtCallbackProc  cancelCB, 
		     XtCallbackProc  applyCB, 
		     XtPointer       clientData,
		     VkSimpleWindow *parent)  { return postModal(msg, okCB, cancelCB, applyCB, clientData, NULL, parent); }

  Widget postModal ( const char *msg, 
		     Widget parent) { return postModal(msg, NULL, NULL, NULL, NULL, NULL, NULL, parent); }
  
  Widget postModal ( const char     *msg, 
		     const char     *helpString, 		       
		     Widget parent) { return postModal(msg, NULL, NULL, NULL, NULL, helpString, NULL, parent); }

  Widget postModal ( const char      *msg, 
		     XtCallbackProc   okCB, 
		     XtPointer        clientData,
		     Widget parent)  { return postModal(msg, okCB, NULL, NULL, clientData, NULL, NULL, parent); }

  Widget postModal ( const char      *msg, 
		     XtCallbackProc   okCB, 
		     XtPointer       clientData,
		     const char     *helpString, 		       
		     Widget parent)  { return postModal(msg, okCB, NULL, NULL, clientData, helpString, NULL, parent); }

  Widget postModal ( const char     *msg, 
		     XtCallbackProc  okCB, 
		     XtCallbackProc  cancelCB, 
		     XtPointer       clientData,
		     Widget parent)  { return postModal(msg, okCB, cancelCB, NULL, clientData, NULL, NULL, parent); }

  Widget postModal ( const char     *msg, 
		     XtCallbackProc  okCB, 
		     XtCallbackProc  cancelCB, 
		     XtPointer       clientData,
		     char            *helpString,
		     Widget parent)  { return postModal(msg, okCB, cancelCB, NULL, clientData, helpString, NULL, parent); }

  Widget postModal ( const char     *msg, 
		     XtCallbackProc  okCB, 
		     XtCallbackProc  cancelCB, 
		     XtCallbackProc  applyCB, 
		     XtPointer       clientData,
		     Widget parent)  { return postModal(msg, okCB, cancelCB, applyCB, clientData, NULL, NULL, parent); }    


// Blocked

  Widget postBlocked ( const char *msg, 
		       VkSimpleWindow *parent) { return postBlocked(msg, NULL, NULL, NULL, NULL, NULL, parent); }
  
  Widget postBlocked ( const char     *msg, 
		       const char     *helpString, 		       
		       VkSimpleWindow *parent) { return postBlocked(msg, NULL, NULL, NULL, NULL, helpString, parent); }

  Widget postBlocked ( const char      *msg, 
		       XtCallbackProc   okCB, 
		       XtPointer        clientData,
		       VkSimpleWindow  *parent = NULL)  { return postBlocked(msg, okCB, NULL, NULL, clientData, NULL, parent); }

  Widget postBlocked ( const char      *msg, 
		       XtCallbackProc   okCB, 
		       XtPointer        clientData,
		       const char     *helpString, 		       
		       VkSimpleWindow  *parent = NULL)  { return postBlocked(msg, okCB, NULL, NULL, clientData, helpString, parent); }


  Widget postBlocked ( const char     *msg, 
		       XtCallbackProc  okCB, 
		       XtCallbackProc  cancelCB, 
		       XtPointer       clientData,
		       VkSimpleWindow *parent = NULL)  { return postBlocked(msg, okCB, cancelCB, NULL, clientData, NULL, parent); }

  Widget postBlocked ( const char     *msg, 
		       XtCallbackProc  okCB, 
		       XtCallbackProc  cancelCB, 
		       XtPointer       clientData,
		       char            *helpString,
		       VkSimpleWindow *parent = NULL)  { return postBlocked(msg, okCB, cancelCB, NULL, clientData, helpString, parent); }

  Widget postBlocked ( const char     *msg, 
		       XtCallbackProc  okCB, 
		       XtCallbackProc  cancelCB, 
		       XtCallbackProc  applyCB, 
		       XtPointer       clientData,
		       VkSimpleWindow *parent)  { return postBlocked(msg, okCB, cancelCB, applyCB, clientData, NULL, parent); }


  Widget postBlocked ( const char *msg, 
		       Widget parent) { return postBlocked(msg, NULL, NULL, NULL, NULL, NULL, NULL, parent); }
  
  Widget postBlocked ( const char     *msg, 
		       const char     *helpString, 		       
		       Widget parent) { return postBlocked(msg, NULL, NULL, NULL, NULL, helpString, NULL, parent); }

  Widget postBlocked ( const char      *msg, 
		       XtCallbackProc   okCB, 
		       XtPointer        clientData,
		       Widget parent)  { return postBlocked(msg, okCB, NULL, NULL, clientData, NULL, NULL, parent); }

  Widget postBlocked ( const char      *msg, 
		       XtCallbackProc   okCB, 
		       XtPointer        clientData,
		       const char     *helpString, 		       
		       Widget parent)  { return postBlocked(msg, okCB, NULL, NULL, clientData, helpString, NULL, parent); }


  Widget postBlocked ( const char     *msg, 
		       XtCallbackProc  okCB, 
		       XtCallbackProc  cancelCB, 
		       XtPointer       clientData,
		       Widget parent)  { return postBlocked(msg, okCB, cancelCB, NULL, clientData, NULL, NULL, parent); }

  Widget postBlocked ( const char     *msg, 
		       XtCallbackProc  okCB, 
		       XtCallbackProc  cancelCB, 
		       XtPointer       clientData,
		       char            *helpString,
		       Widget parent)  { return postBlocked(msg, okCB, cancelCB, NULL, clientData, helpString, NULL, parent); }

  Widget postBlocked ( const char     *msg, 
		       XtCallbackProc  okCB, 
		       XtCallbackProc  cancelCB, 
		       XtCallbackProc  applyCB, 
		       XtPointer       clientData,
		       Widget parent)  { return postBlocked(msg, okCB, cancelCB, applyCB, clientData, NULL, NULL, parent); }

    
  // Doesn't return until choice has been made. Returns the user's choice

  VkDialogReason postAndWait ( const char      *message, 
			       const char      *helpString, 		       
			       VkSimpleWindow  *parent     = NULL);
  
  VkDialogReason postAndWait ( const char      *message, 
			       VkSimpleWindow  *parent) { return postAndWait(message, NULL, parent); }

  VkDialogReason postAndWait ( const char      *message, 
			       const char      *helpString, 		       
			       Widget          parent);
  
  VkDialogReason postAndWait ( const char      *message, 
			       Widget          parent) { return postAndWait(message, NULL, parent); }    


  private:

    static void VkDialogMapped (Widget, XtPointer, XtPointer);
    static void okXtCallback (Widget, XtPointer, XtPointer);
    static void applyXtCallback (Widget, XtPointer, XtPointer);
    static void cancelXtCallback (Widget, XtPointer, XtPointer);
    static void helpXtCallback (Widget, XtPointer, XtPointer);

    static void handleWMDeleteCallback (Widget, XtPointer, XtPointer);

    Widget _widgetParent;
    void help(const char *);
    VkWidgetList  _widgets;

    Widget findDialog(Widget parent);

    VkDialog * getCacheInfo(Widget);

    Widget findValidParent(VkSimpleWindow *);

    char    *_nextTitle;
    char    *_nextOKLabel, 
            *_nextCancelLabel, 
            *_nextApplyLabel;

    Boolean _centerMode;

    VkDialog     *_dialogList;

    void clearCache(Widget);


};

#endif



