#ifndef _SGZOOMBARP_H_
#define _SGZOOMBARP_H_

#include <Sgm/ZoomBar.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ZoomBar class structure */

typedef struct _SgZoomBarClassPart
{
   int foo;
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgZoomBarClassPart;


/* Full class record declaration for ZoomBar class */

typedef struct _SgZoomBarClassRec {
    CoreClassPart	  core_class;
    XmPrimitiveClassPart  primitive_class;
   SgZoomBarClassPart    zoombar_class;
} SgZoomBarClassRec;


externalref SgZoomBarClassRec sgZoomBarClassRec;

/* ZoomBar instance record */

typedef struct _SgZoomBarPart
{
   XtCallbackList   activate_callback;
   XtCallbackList   arm_callback;
   XtCallbackList   disarm_callback;

   Boolean 	    armed;
   int		    click_count;
   
/* Below here is stuff added for ZoomBar */

   Dimension *      section_widths;       /* array which divides up button */
   int              section_count;	  /* number of divisions in section_widths*/
   int              pushed_section;	  /* the section which was last pushed */
   int              located_section;	  /* the section which was last located */

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgZoomBarPart;


/* Full instance record declaration */

typedef struct _SgZoomBarRec {
    CorePart	     core;
    XmPrimitivePart  primitive;
    SgZoomBarPart    zoomBar;
} SgZoomBarRec;


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SGZOOMBUTTONP_H_ */
/* DON'T ADD ANYTHING AFTER THIS #endif */

