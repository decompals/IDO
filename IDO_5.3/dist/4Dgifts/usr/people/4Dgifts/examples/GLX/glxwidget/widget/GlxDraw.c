/* $Header: /proj/irix5.3/isms/4Dgifts/examples/GLX/glxwidget/widget/RCS/GlxDraw.c,v 1.7 1993/02/16 08:44:18 mitch Exp $ */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifdef __GLX_MOTIF
#include <Xm/PrimitiveP.h>
#include <X11/Xirisw/GlxMDrawP.h>
#else /* not __GLX_MOTIF */
#include <X11/Xirisw/GlxDrawP.h>
#endif /* __GLX_MOTIF */

#ifdef __GLX_MOTIF
/* The MOTIF version differs only in the inclusion of the primitive
 * widget class and in a vew variable and type name differences.
 * Rather than put ifdefs all over the place, we just use a few defines
 * to make it use motif types and names
 */
#define GlxDrawWidget	GlxMDrawWidget
#define GlxDrawClassRec	GlxMDrawClassRec
#define glxDrawClassRec	glxMDrawClassRec
#define glxDrawWidgetClass	glxMDrawWidgetClass
#define GlxDrawRec		GlxMDrawRec
#endif /* __GLX_MOTIF */

#define offset(field) XtOffset(GlxDrawWidget, glxDraw.field)

static char defaultTranslations[] =
#ifdef __GLX_MOTIF
     "<Key>osfHelp:PrimitiveHelp() \n"
#endif
    "<KeyDown>:	glxInput() \n\
     <KeyUp>:	glxInput() \n\
     <BtnDown>: glxInput() \n\
     <BtnUp>:   glxInput() \n\
     <BtnMotion>: glxInput() ";

static void glxInput();
static void auxwindow_destroyed();

static GLXconfig default_config[] = {{0,0,0}};
static XtActionsRec actions[] = {
    { "glxInput", glxInput },	/* key or mouse input */
};

static XtResource resources[] = {
  {GlxNglxConfig, GlxCGlxConfig, GlxRGlxConfig, sizeof(GLXconfig *),
       offset(config),
       XtRImmediate, (caddr_t) default_config},

  {GlxNginitCallback, GlxCCallback, XtRCallback, sizeof (XtCallbackList),
       offset(ginit_callback), XtRImmediate, (caddr_t) NULL},

  {GlxNinputCallback, GlxCCallback, XtRCallback, sizeof (XtCallbackList),
       offset(input_callback), XtRImmediate, (caddr_t) NULL},

  {GlxNresizeCallback, GlxCCallback, XtRCallback, sizeof (XtCallbackList),
       offset(resize_callback), XtRImmediate, (caddr_t) NULL},

  {GlxNexposeCallback, GlxCCallback, XtRCallback, sizeof (XtCallbackList),
       offset(expose_callback), XtRImmediate, (caddr_t) NULL},

  {GlxNvisual, GlxCVisual, GlxRVisualInfo, sizeof (XVisualInfo *),
       offset(visualInfo), XtRImmediate, (caddr_t) NULL},

  {GlxNoverrideColormap, GlxCOverrideColormap, XtRBoolean, sizeof (Boolean),
       offset(override_colormap), XtRImmediate, (caddr_t) TRUE},

  {GlxNuseOverlay, GlxCUseOverlay, XtRBoolean, sizeof (Boolean),
       offset(overlay_info.exists), XtRImmediate, (caddr_t) FALSE},

  {GlxNoverlayWindow, GlxCWindow, XtRWindow, sizeof (Window),
       offset(overlay_info.window), XtRImmediate, (caddr_t) 0},

  {GlxNoverlayColormap, GlxCColormap, XtRColormap, sizeof (Colormap),
       offset(overlay_info.colormap), XtRImmediate, (caddr_t) 0},

  {GlxNoverlayDepth, GlxCDepth, XtRInt, sizeof (int),
       offset(overlay_info.depth), XtRImmediate, (caddr_t) 0},

  {GlxNoverlayVisual, GlxCVisual, GlxRVisualInfo, sizeof (XVisualInfo *),
       offset(overlay_info.visualInfo), XtRImmediate, (caddr_t) 0},

  {GlxNoverlayExposeCallback, GlxCCallback, XtRCallback,
       sizeof (XtCallbackList),
       offset(overlay_info.expose_callback), XtRImmediate, (caddr_t) NULL},

  {GlxNuseUnderlay, GlxCUseUnderlay, XtRBoolean, sizeof (Boolean),
       offset(underlay_info.exists), XtRImmediate, (caddr_t) FALSE},

  {GlxNunderlayWindow, GlxCWindow, XtRWindow, sizeof (Window),
       offset(underlay_info.window), XtRImmediate, (caddr_t) 0},

  {GlxNunderlayColormap, GlxCColormap, XtRColormap, sizeof (Colormap),
       offset(underlay_info.colormap), XtRImmediate, (caddr_t) 0},

  {GlxNunderlayDepth, GlxCDepth, XtRInt, sizeof (int),
       offset(underlay_info.depth), XtRImmediate, (caddr_t) 0},

  {GlxNunderlayVisual, GlxCVisual, GlxRVisualInfo, sizeof (XVisualInfo *),
       offset(underlay_info.visualInfo), XtRImmediate, (caddr_t) 0},

  {GlxNunderlayExposeCallback, GlxCCallback, XtRCallback,
       sizeof (XtCallbackList),
       offset(underlay_info.expose_callback), XtRImmediate, (caddr_t) NULL},

  {GlxNusePopup, GlxCUsePopup, XtRBoolean, sizeof (Boolean),
       offset(popup_info.exists), XtRImmediate, (caddr_t) FALSE},

  {GlxNpopupWindow, GlxCWindow, XtRWindow, sizeof (Window),
       offset(popup_info.window), XtRImmediate, (caddr_t) 0},

  {GlxNpopupColormap, GlxCColormap, XtRColormap, sizeof (Colormap),
       offset(popup_info.colormap), XtRImmediate, (caddr_t) 0},

  {GlxNpopupDepth, GlxCDepth, XtRInt, sizeof (int),
       offset(popup_info.depth), XtRImmediate, (caddr_t) 0},

  {GlxNpopupVisual, GlxCVisual, GlxRVisualInfo, sizeof (XVisualInfo *),
       offset(popup_info.visualInfo), XtRImmediate, (caddr_t) 0},

  {GlxNpopupExposeCallback, GlxCCallback, XtRCallback,
       sizeof (XtCallbackList),
       offset(popup_info.expose_callback), XtRImmediate, (caddr_t) NULL},

#ifdef __GLX_MOTIF
  /* Primitive resources */
  {XmNtraversalOn, XmCTraversalOn, XmRBoolean, sizeof (Boolean),
       XtOffset (GlxDrawWidget, primitive.traversal_on), XmRImmediate,
       (caddr_t) FALSE},
  
  {XmNhighlightOnEnter, XmCHighlightOnEnter, XmRBoolean, sizeof (Boolean),
       XtOffset (GlxDrawWidget, primitive.highlight_on_enter), XmRImmediate,
       (caddr_t) FALSE},
  
  {XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
       sizeof (Dimension),
       XtOffset (GlxDrawWidget, primitive.shadow_thickness), XmRImmediate,
       (caddr_t) 0},
  
  {XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
       sizeof (Dimension),
       XtOffset (GlxDrawWidget, primitive.highlight_thickness), XmRImmediate,
       (caddr_t) 0},

#endif /* __GLX_MOTIF */
  
#undef offset
};

static void
    ClassInitialize(),
    Initialize(),
    Realize(),
    CreateWindow(),
    Redraw(),
    Resize(),
    Destroy();
static Boolean
    SetValues();

GlxDrawClassRec glxDrawClassRec =
{
  { /* core fields */
#ifdef __GLX_MOTIF
    /* superclass		*/	(WidgetClass) &xmPrimitiveClassRec,
    /* class_name		*/	"GlxMDraw",
#else /* not __GLX_MOTIF */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"GlxDraw",
#endif /* __GLX_MOTIF */
    /* widget_size		*/	sizeof(GlxDrawRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	TRUE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	Redraw,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	defaultTranslations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
#ifdef __GLX_MOTIF /* primitive resources */
  {
    /* border_highlight		*/	XmInheritBorderHighlight,
    /* border_unhighlight	*/	XmInheritBorderUnhighlight,
    /* translations		*/	XtInheritTranslations,
    /* arm_and_activate		*/	NULL,
    /* get_resources		*/	NULL,
    /* num get_resources	*/	0,
    /* extension		*/	NULL,				
  }
#endif /* __GLX_MOTIF */
};

WidgetClass glxDrawWidgetClass = (WidgetClass)&glxDrawClassRec;

static error(w,string)
Widget w;
char *string;
{
    char buf[100];
#ifdef __GLX_MOTIF
    sprintf (buf, "GlxMDraw: %s\n", string);
#else
    sprintf (buf, "GlxDraw: %s\n", string);
#endif
    XtAppError(XtWidgetToApplicationContext(w), buf);
}

static warning(w,string)
Widget w;
char *string;
{
    char buf[100];
#ifdef __GLX_MOTIF
    sprintf (buf, "GlxMDraw: %s\n", string);
#else
    sprintf (buf, "GlxDraw: %s\n", string);
#endif
    XtAppWarning(XtWidgetToApplicationContext(w), buf);
}

static void ClassInitialize()
{
}

static void Initialize (req, new)
    GlxDrawWidget req, new;
{
    GLXconfig *newConfig;
    GlxDrawWindowInfo normalWindowInfo;
    GlxDrawWindowInfo *curWindowInfo;
    XVisualInfo tmpl;
    register i;
    int nret;
    XSetWindowAttributes aux_attributes;
   
    if (req->core.width == 0)
	new->core.width = 100;
    if (req->core.height == 0)
	new->core.width = 100;
    bzero (&normalWindowInfo, sizeof (normalWindowInfo));
    normalWindowInfo.exists = TRUE;
    if (!new->glxDraw.override_colormap)
	normalWindowInfo.colormap = new->core.colormap;
    newConfig = GLXgetconfig (XtDisplay(new),
			      XScreenNumberOfScreen(XtScreen(new)),
			      new->glxDraw.config);
    if (!newConfig)
    {
	error(new,"requested visual not supported");
	return;
    }
    new->glxDraw.config = newConfig;
    for (i=0; newConfig[i].buffer; i++)
    {
	switch (newConfig[i].buffer)
	{
	case GLX_NORMAL:
	    curWindowInfo = &normalWindowInfo;
	    break;
	case GLX_OVERLAY:
	    curWindowInfo = &new->glxDraw.overlay_info;
	    break;
	case GLX_UNDERLAY:
	    curWindowInfo = &new->glxDraw.underlay_info;
	    break;
	case GLX_POPUP:
	    curWindowInfo = &new->glxDraw.popup_info;
	    break;
	}
	if (!curWindowInfo->exists)
	    continue;
	switch (newConfig[i].mode)
	{
	case GLX_COLORMAP:
	    if (!curWindowInfo->colormap)
		curWindowInfo->colormap = (Colormap)newConfig[i].arg;
	    break;
	case GLX_VISUAL:
	    tmpl.visualid = newConfig[i].arg;
	    tmpl.screen = XScreenNumberOfScreen(XtScreen(new));
	    curWindowInfo->visualInfo =
		XGetVisualInfo(XtDisplay(new),VisualScreenMask|VisualIDMask,
		&tmpl, &nret);
	    if (!curWindowInfo->visualInfo)
		error (new,"Couldn't get visual");
	    curWindowInfo->depth = curWindowInfo->visualInfo->depth;
	    break;
	}
    }
    if (new->glxDraw.override_colormap)
	new->core.colormap = normalWindowInfo.colormap;
    new->glxDraw.visualInfo = normalWindowInfo.visualInfo;
    new->core.depth = normalWindowInfo.visualInfo->depth;
#ifdef __GLX_MOTIF
    /* The following resources cannot be changed by the user */
    new->primitive.highlight_thickness = 0;
    new->primitive.shadow_thickness = 0;
#endif /* __GLX_MOTIF */
}

/* set values portion for the auxiliaries */
static void auxSetValues (w, current, request, new)
Widget w; /* for obtaining the display */
GlxDrawWindowInfo *current, *request, *new;
{
    XSetWindowAttributes attributes;

    /* Changing the following after create time will harm me */
    new->exists = current->exists;
    new->window = current->window;
    new->depth = current->depth;
    new->visualInfo = current->visualInfo;

    /* if the colormap has changed, and the window has been
     * created, change the colormap on the window */
    if (current->colormap != new->colormap && new->window)
    {
	attributes.colormap = new->colormap;
	XChangeWindowAttributes(
		XtDisplay(w), new->window, CWColormap, &attributes);
    }
}

static Boolean SetValues (current, request, new)
GlxDrawWidget current, request, new;
{
    auxSetValues(new,
		 &current->glxDraw.overlay_info,
		 &request->glxDraw.overlay_info,
		 &new->glxDraw.overlay_info);
    auxSetValues(new,
		 &current->glxDraw.underlay_info,
		 &request->glxDraw.underlay_info,
		 &new->glxDraw.underlay_info);
    auxSetValues(new,
		 &current->glxDraw.popup_info,
		 &request->glxDraw.popup_info,
		 &new->glxDraw.popup_info);
#ifdef __GLX_MOTIF
    /* The following resources cannot be changed by the user */
    new->primitive.highlight_thickness = 0;
    new->primitive.shadow_thickness = 0;
#endif /* __GLX_MOTIF */
    return (FALSE);
}
    
static void realizeAux(glw,aux_info)
register GlxDrawWidget glw;
register GlxDrawWindowInfo *aux_info;
{
   XSetWindowAttributes aux_attributes;

   aux_attributes.colormap = aux_info->colormap;
   aux_attributes.border_pixel = 0;
   aux_info->window =
	    XCreateWindow(XtDisplay(glw), XtWindow(glw), 0, 0,
			  glw->core.width, glw->core.height, 0,
			  aux_info->depth, InputOutput,
			  aux_info->visualInfo->visual,
			  CWColormap|CWBorderPixel, &aux_attributes);
   XMapWindow(XtDisplay(glw),aux_info->window);
   /* We explicitly need the exposure events for this window */
   XSelectInput(XtDisplay(glw), aux_info->window,	ExposureMask);
   /* Tell Xt to tell us about the exposure events
    * Note that this is a private Xt routine that may change in the
    * future, but it is the only way to get the exposure events.  This
    * code may need to change in the future
    */
   _XtRegisterWindow(aux_info->window,glw);
}

static void Realize(w, valueMask, attributes)
    Widget w;
    Mask *valueMask;
    XSetWindowAttributes *attributes;
{
    register GlxDrawWidget glw = (GlxDrawWidget)w;
    Boolean has_aux=FALSE;
    GlxDrawCallbackStruct cb;
    register GLXconfig *glp;
   
    /* Since GL programs are expected to clear the screen themselves,
     * we don't want X to clear the screen, so don't set the background
     * pixel.
     */
    *valueMask &= ~CWBackPixel;
    
    if (glw->glxDraw.override_colormap)
    {
	*valueMask |= CWColormap;
	attributes->colormap = glw->core.colormap;
    }
    XtCreateWindow (w, (unsigned int)InputOutput,
		    glw->glxDraw.visualInfo->visual,
		    *valueMask, attributes);
    if (glw->glxDraw.overlay_info.exists)
    {
	has_aux = TRUE;
	realizeAux(glw,&glw->glxDraw.overlay_info);
    }
    if (glw->glxDraw.underlay_info.exists)
    {
	has_aux = TRUE;
	realizeAux(glw,&glw->glxDraw.underlay_info);
    }
    if (glw->glxDraw.popup_info.exists)
    {
	has_aux = TRUE;
	realizeAux(glw,&glw->glxDraw.popup_info);
    }
    /* If there is an overlay or an underlay, we need to be notified if it
     * is destroyed */
    if (has_aux)
	XtAddEventHandler((Widget)glw, SubstructureNotifyMask, FALSE,
			  auxwindow_destroyed, NULL);

    for (glp = glw->glxDraw.config; glp->buffer; glp++)
    {
	if (glp->mode == GLX_WINDOW)
	{
	    switch (glp->buffer)
	    {
	    case GLX_NORMAL:
		glp->arg = XtWindow(glw);
		break;
	    case GLX_OVERLAY:
		if (glw->glxDraw.overlay_info.exists)
		    glp->arg = glw->glxDraw.overlay_info.window;
		break;
	    case GLX_UNDERLAY:
		if (glw->glxDraw.underlay_info.exists)
		    glp->arg = glw->glxDraw.underlay_info.window;
		break;
	    case GLX_POPUP:
		if (glw->glxDraw.popup_info.exists)
		    glp->arg = glw->glxDraw.popup_info.window;
		break;
	    }
	}
    }
    GLXlink(XtDisplay(w), glw->glxDraw.config);
    /* Be a nice guy and do one GLXwinset on the main window.  This avoids
     * the need to do GLXwinsets in programs with only one window.
     */
    GLXwinset(XtDisplay(w), XtWindow(w));
    cb.reason = GlxCR_GINIT;
    cb.event = NULL;
    cb.window = XtWindow(glw);
    cb.width = glw->core.width;
    cb.height = glw->core.height;
    XtCallCallbackList((Widget) glw, glw->glxDraw.ginit_callback, &cb);
}

static void
Redraw (glw, event, region)
    GlxDrawWidget	glw;
    XEvent	*event;
    Region	region;
{
   GlxDrawCallbackStruct cb;
   XtCallbackList cblist;
   
   cb.reason = GlxCR_EXPOSE;
   cb.event = event;
   cb.window = event->xexpose.window;
   cb.width = glw->core.width;
   cb.height = glw->core.height;
   if (cb.window == XtWindow(glw))
   {
       cb.buffer = GLX_NORMAL;
       cblist = glw->glxDraw.expose_callback;
   }
   else if (cb.window == glw->glxDraw.overlay_info.window)
   {
       cb.buffer = GLX_OVERLAY;
       cblist = glw->glxDraw.overlay_info.expose_callback;
   }
   else if (cb.window == glw->glxDraw.underlay_info.window)
   {
       cb.buffer = GLX_UNDERLAY;
       cblist = glw->glxDraw.underlay_info.expose_callback;
   }
   else if (cb.window == glw->glxDraw.popup_info.window)
   {
       cb.buffer = GLX_POPUP;
       cblist = glw->glxDraw.popup_info.expose_callback;
   }
   else
   {
       warning ("Unknown window in redraw callback");
       cb.buffer = GLX_NORMAL;  /* I have to set it to something */
       cblist = glw->glxDraw.expose_callback;
   }
   XtCallCallbackList ((Widget) glw, cblist, &cb);
}

static void
Resize(glw)
    GlxDrawWidget glw;
{
    GlxDrawCallbackStruct cb;

    /* if we get a resize event before being realized, we can't handle it */
    if (!XtIsRealized((Widget)glw))
	return;
    if (glw->glxDraw.overlay_info.exists && glw->glxDraw.overlay_info.window)
	XResizeWindow(XtDisplay(glw), glw->glxDraw.overlay_info.window,
		      glw->core.width, glw->core.height);
    if (glw->glxDraw.underlay_info.exists && glw->glxDraw.underlay_info.window)
	XResizeWindow(XtDisplay(glw), glw->glxDraw.underlay_info.window,
		      glw->core.width, glw->core.height);
    if (glw->glxDraw.popup_info.exists && glw->glxDraw.popup_info.window)
	XResizeWindow(XtDisplay(glw), glw->glxDraw.popup_info.window,
		      glw->core.width, glw->core.height);
    cb.reason = GlxCR_RESIZE;
    cb.event = NULL;
    cb.window = XtWindow((Widget) glw);
    cb.width = glw->core.width;
    cb.height = glw->core.height;
    cb.buffer = GLX_NORMAL;
    /* The following XSync fixes a bug whereby viewport can fail.
     * On Indigo, viewport queries the size of the X window
     * and gets the old size without this Xsync.
     */
    XSync(XtDisplay(glw),FALSE);
    XtCallCallbackList ((Widget) glw, glw->glxDraw.resize_callback, &cb);
}

static void
Destroy(glw)    
    GlxDrawWidget glw;
{
    XtFree((char *)glw->glxDraw.config);
    if (glw->glxDraw.overlay_info.exists)
    {
	_XtUnregisterWindow(glw->glxDraw.overlay_info.window,glw);
	glw->glxDraw.overlay_info.exists = FALSE;
	glw->glxDraw.overlay_info.window = NULL;
	/*The overlay window will be automatically destroyed when the
	 *widget window is actually destroyed
	 */
    }
    if (glw->glxDraw.underlay_info.exists)
    {
	_XtUnregisterWindow(glw->glxDraw.underlay_info.window,glw);
	glw->glxDraw.underlay_info.exists = FALSE;
	glw->glxDraw.underlay_info.window = NULL;
	/*The underlay window will be automatically destroyed when the
	 *widget window is actually destroyed
	 */
    }
    if (glw->glxDraw.popup_info.exists)
    {
	_XtUnregisterWindow(glw->glxDraw.popup_info.window,glw);
	glw->glxDraw.popup_info.exists = FALSE;
	glw->glxDraw.popup_info.window = NULL;
	/*The popup window will be automatically destroyed when the
	 *widget window is actually destroyed
	 */
    }
    GLXunlink(XtDisplay((Widget) glw), XtWindow((Widget) glw));
}

/* Action routine for keyboard and mouse events */
/* ARGSUSED */
static void glxInput (glw, event, params, num_params)
    GlxDrawWidget glw;
    XEvent *event;
    String *params;			/* unused */
    Cardinal *num_params;		/* unused */
{
   GlxDrawCallbackStruct cb;
   
   cb.reason = GlxCR_INPUT;
   cb.event = event;
   cb.window = XtWindow(glw);
   cb.buffer = GLX_NORMAL;
   cb.width = glw->core.width;
   cb.height = glw->core.height;
   XtCallCallbackList ((Widget) glw, glw->glxDraw.input_callback, &cb);
}

/* If the user explicitly destroys the overlay or underlay windows, clean up */
static void auxwindow_destroyed(glw, client_data, event)
GlxDrawWidget glw;
caddr_t client_data;
XEvent *event;
{
    if (event->type == DestroyNotify && event->xdestroywindow.window)
    {
	if (glw->glxDraw.overlay_info.exists &&
	    event->xdestroywindow.window == glw->glxDraw.overlay_info.window)
	{
	    _XtUnregisterWindow(glw->glxDraw.overlay_info.window,glw);
	    glw->glxDraw.overlay_info.exists = FALSE;
	    glw->glxDraw.overlay_info.window = 0;
	}
	if (glw->glxDraw.underlay_info.exists &&
	    event->xdestroywindow.window == glw->glxDraw.underlay_info.window)
	{
	    _XtUnregisterWindow(glw->glxDraw.underlay_info.window,glw);
	    glw->glxDraw.underlay_info.exists = FALSE;
	    glw->glxDraw.underlay_info.window = 0;
	}
	if (glw->glxDraw.popup_info.exists &&
	    event->xdestroywindow.window == glw->glxDraw.popup_info.window)
	{
	    _XtUnregisterWindow(glw->glxDraw.popup_info.window,glw);
	    glw->glxDraw.popup_info.exists = FALSE;
	    glw->glxDraw.popup_info.window = 0;
	}
    }
}

#ifdef __GLX_MOTIF
/* Provide a Motif-style create routine */
Widget GlxCreateMDraw(parent, name, arglist, argcount)
Widget parent;
char *name;
ArgList arglist;
Cardinal argcount;
{
    return (XtCreateWidget (name, glxMDrawWidgetClass, parent, arglist,
			    argcount));
}
#endif
