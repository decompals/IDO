#ifndef __OglColorSliderManager_h_
#define __OglColorSliderManager_h_

#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <GL/gl.h>
#include <GL/GLwMDrawA.h>
#include <Sgm/OglColorSliderManagerC.h>
#include <Sgm/SgColor.h>
#include <Sgm/SgList.h>


// -------------------------------------------------------------------------
// Ogl Manager that manages several slider gadgets
//

class _SgOglColorSliderGadget;

class _SgOglColorSliderManager : public SgColor {
  public:
    _SgOglColorSliderManager(Widget parent, const char* instanceName, 
			 Arg *args, Cardinal num_args);
    ~_SgOglColorSliderManager();

    virtual void render();
    void expose();
    virtual void processEvent(GLwDrawingAreaCallbackStruct *call_data);

    virtual void initSlider(Display *d, XVisualInfo *v);

    GLXContext getContext()   { return glx_context;}
    Widget getWidget()              { return gl_widget; }
    void   addCallback(XtCallbackProc callback, void *client_data); 
    // Add a gadget slider to the Ogl manager
    void   addGadget(_SgOglColorSliderGadget *after, _SgOglColorSliderGadget *gadget);
    void   deleteGadget(_SgOglColorSliderGadget *gadget);
    void   addGadgets(_SgOglColorSliderGadget *after,
		      _SgOglColorSliderGadget **gadget, int num_gadgets);
    void   deleteGadgets(_SgOglColorSliderGadget **gadget, int num_gadgets);
    Dimension getWidth()   { return(gl_widget->core.width); }
    
    void   winset()  { GLwDrawingAreaMakeCurrent(getWidget(), getContext()); }   

    void drawSgOglBackground();	// Draw the Ogl widget background
    void setWorkProcRun(Boolean val) { work_proc_run = val; }

  protected:
    void    resize();		// calculate and set children positions
    _SgOglColorSliderGadget *calculateSliderGadget(XEvent* event);
    Boolean getDoubleBuf() { return(True); }
    Dimension getHeight()  { return(gl_widget->core.height); }
    _SgOglColorSliderGadget *selected_slider; // Current events are is this slider
    Widget              gl_widget; // The slider is drawn on a Motif Ogl widget
    Boolean             rgbmode;
    XtCallbackProc      callback;
    void               *client_data;
    SgList             *gadgets; // List of slider gadgets
    Boolean             work_proc_run; // Flag whether the work proc is invoked

    short		r, g, b;
    short		minr, ming, minb;
    short		maxr, maxg, maxb;
    Bool		picking;
    float		value;		// current value of the V slider.
    GLXContext          glx_context;

};

#endif /* __OglColorSliderManager_h_ */
