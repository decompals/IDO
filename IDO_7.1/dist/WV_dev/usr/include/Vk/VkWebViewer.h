// -*- c++ -*-
////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1996  Silicon Graphics, Inc.  All Rights Reserved.   ///////
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
#ifndef VkWebViewer_H
#define VkWebViewer_H
#include <Vk/VkComponent.h>
#include <sys/param.h>

typedef enum { SG_FIRST, SG_LAST, SG_PREV, SG_NEXT } VkWebViewerDirection;
typedef enum { SG_POST, SG_GET } VkWebViewerMethod;
typedef enum { SG_NONE, SG_MINIMAL, SG_FULL} VkWebViewerControlsEnum;

class VkOptionMenu;
class VkMenuAction;
class VkMenuToggle;
class VkMenuItem;
class VkWebViewerDocument;
class VkWebViewerData;
class VkWebViewerBaseData;

class VkWebViewerBase : public VkComponent
{
public:

  VkWebViewerBase(const char *, VkComponent);
  VkWebViewerBase(const char *, Widget);
  VkWebViewerBase(const char *);
  virtual ~VkWebViewerBase();
  void create ( Widget );
  const char *  className();
    

protected:
  VkWebViewerBaseData*   _base;

  Widget  _scrolledw;
  Widget  _viewer;
  Widget  _vertSB;
  Widget  _horizSB;

  // Information about the current document
  char*		     currentAnchor;
  VkWebViewerDocument* currentDoc;
  char*		     requestQuery;

  // Loading information
  char*		     file_tmp_string;
  XtPointer	     load_context;
  int		     num_simul; // Number of simultaneous image loads
  static int	     filecounter;
  static int	     webLibInitLevel;

  // State
  Boolean	     flush_images;
  Boolean	     follow_links;
  Boolean	     reloading;

private:
  // Private data -  Array of default resources
  static String      _defaultVkWebViewerBaseResources[];

  //
  // Methods
  //

protected:
  // Called by the base constructor
  void		web_init();
  void		web_destroy();

  // Handle reading documents
  static void DataHeaderCB(void *param);
  static void DataDoneCB(void *param);
  static void DataCB(void *param);

  // General data reader interfact for use with Wio
  // and XtAppAddInput
  static void GetDocument(XtPointer, int*, XtInputId*);

  // Handle reading images
  static void ImageTimer(XtPointer, XtIntervalId*);
  static void ImageSpawn(XtPointer);
  static void ImageCB(Widget, XtPointer, XtPointer);
  static void ImageDoneCB(void*);
  static void ImageHeaderCB(void *);
  static void ImageLoadedCB(void *);

  // Handle drag and drop
  static void ButtonHandler(Widget, XtPointer, XEvent*, Boolean*);
  static Boolean ConvertHandler(Widget, Atom*, Atom*, Atom*, 
				XtPointer*, unsigned long*, int*);
  static void DropCB(Widget, XtPointer, XtPointer);
  static void SelectionCB(Widget, XtPointer, Atom*, Atom*, XtPointer, 
			  unsigned long*, int*);

  // Form submission 
  static void FormCB(Widget, XtPointer, XtPointer);

  // General callbacks for tying to controls and such
  static void ActivateCB(Widget, XtPointer, XtPointer);
  static void BusyCB(Widget, XtPointer, XtPointer);
  static void HrefCB(Widget, XtPointer, XtPointer);

protected:

  virtual void	purgeCache();

  virtual void	activate(XtPointer);
  virtual void	formActivate(const char* url, const char* query, 
			     const char* method);
  virtual Boolean  documentLoaded(void *vcb);
  virtual void	notify(const char*);
  virtual void	showBusy(Boolean);
  virtual void  changingPage();

  void		dnd_setup();
  char*		canonicalizeUrl(const char*);
  Widget	getJumperIcon();
  long				default_expiration;

public:
  virtual void	stopTransfer(); // Stop in progress web access

  virtual void	setURL(const char* url);
  virtual void	loadFile(const char* filename);
  virtual void	setQuery(const char* s);
  virtual void	gotoLink(const char* url, 
			 VkWebViewerMethod method = SG_GET);

  virtual void	setSimulCount(unsigned int n) {
    if (n < 1) n = 1; num_simul = n; }
  virtual unsigned int	getSimulCount() { return num_simul; }

  virtual void	setFileCacheDir(char *pathname);
  virtual void	setFileCacheSize(unsigned int ns);
  virtual void  setFileCacheItemSizeLimit(unsigned long ns);
  virtual unsigned long getFileCacheItemSizeLimit();
  virtual unsigned int	getFileCacheSize();
  virtual void	setDefaultExpiration(time_t);
  virtual time_t getDefaultExpiration();
  virtual void setFollowLinks(Boolean f) { follow_links = f; }
  virtual Boolean getFollowLinks() { return follow_links; }
  char*		findCacheFile();

  virtual void	postError(const char*);
};

class VkWebViewer : public VkWebViewerBase
{ 
  friend class VkWebViewerBase;
public:

  VkWebViewer(const char *, VkComponent);
  VkWebViewer(const char *, Widget);
  VkWebViewer(const char *);
  virtual ~VkWebViewer();
  void create ( Widget );
  const char *  className();
    
  static VkComponent *CreateVkWebViewer( const char *name, Widget parent );

protected:

  VkWebViewerData* _viewer_e;

  // Widgets created by this class
    
  Widget  _backArrow;
  Widget  _forwardArrow;
  Widget  _home;
  Widget  _reload;
  Widget  _label;
  Widget  _stop_button;
  Widget  _optionMenu;
  Widget  _optionMenuPulldown;
    
private:
  // Private data -  Array of default resources
  static String      _defaultVkWebViewerResources[];
    
protected:

  unsigned int	     currentURL; // Index into urls_visited below
  int		     lastURL;
  int		     valid_om_items;
    
private:
    
  static void *RegisterVkWebViewerInterface();

protected:
  virtual void	notify(const char*);
  virtual void	showBusy(Boolean);
  virtual void  changingPage();
  virtual Boolean  documentLoaded(void *vcb);
  static void StopCB(Widget, XtPointer, XtPointer);
  static void ForwardCB(Widget, XtPointer, XtPointer);
  static void BackwardCB(Widget, XtPointer, XtPointer);
  static void ItemCB(Widget, XtPointer, XtPointer);
  static void ReloadCB(Widget, XtPointer, XtPointer);

public:
  virtual void	navigate(VkWebViewerDirection direction);
  virtual void	navigate(unsigned int i);
  virtual void  resetStack();
  virtual unsigned int  getStackDepth() { return lastURL; };
  virtual void	setControls(Boolean); // True to show, False to hide
  virtual void	setControls(VkWebViewerControlsEnum);

};

//
// Class VkWVAVPair
//
// Used to store form information 
//
class VkWebViewerAVPair {
public:
  VkWebViewerAVPair() {};
  virtual ~VkWebViewerAVPair() {} ;
};

class VkWebViewerDocument {
public:
  // No instance methods,  just functions for data access 
  // in subclasses
  unsigned int	formCount();
  void*		getForm(unsigned int);
  // Iter should be a pointer to NULL to start,  formGetData will allocate an iter and
  // free it after the last pair is returned.  This call will return NULL
  // like this:
  // iter_variable = NULL;
  // ... formGetData(form, &iter_variable);
  VkWebViewerAVPair *formGetData(void* form, void** iter);
  // Pairs should be freed after calls to formSetData
  void		formSetData(void* form, VkWebViewerAVPair*);
};

#endif

