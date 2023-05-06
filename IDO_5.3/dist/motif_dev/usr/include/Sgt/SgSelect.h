#ifndef _SGSELECT_H_
#define _SGSELECT_H_

/*
 * File:       SgSelect.h
 *
 * This file defines the private C language API for selection and for 
 * selection data type converters.
 *
 */

#include "SgtSelect.h"

#ifdef __cplusplus
extern "C" {
#endif 
  
  
/*
 * Internal routines from Selection.c which are needed by SgSelection
 *
 */
  
#if defined (__sgi) && defined(ENHANCE_SELECT)
extern
  void _SgtGetSelectionValue(
  Widget widget,
  Atom selection,
  Atom target,
  XtSelectionCallbackProc callback,
  XtPointer closure,
  Time time,
  Boolean incremental
			 );
  
extern
  Boolean _SgtOwnSelection(
		       Widget widget,
		       Atom selection,
		       Time time,
		       XtConvertSelectionProc convert,
		       XtLoseSelectionProc lose,
		       XtSelectionDoneProc notify,
		       XtCancelConvertSelectionProc cancel,
		       XtPointer closure,
		       Boolean incremental
		       );
  
#endif /* __sgi && ENHANCE_SELECT */   

/*
 *  The following routines are designed to form an API which is equivalent to the
 *  XtSelection API.
 */
  
/* Modeled after XtOwnSelection */
  Boolean 
    SgOwnSelection(Widget                   w, 
		   Atom                     selection, 
		   Time                     time,
		   XtConvertSelectionProc   convert_callback,
		   XtLoseSelectionProc      lose_callback,
		   XtSelectionDoneProc      done_callback);
  
/* Modeled after XtGetSelectionValue */
  void 
    SgGetSelectionValue(Widget                      w, 
			Atom                        selection,
			Atom                        target, 
			XtSelectionCallbackProc     callback,
			XtPointer                   client_data,
			Time                        time );
  
/* Modeled after XtGetSelectionValues */
/*
 *  Not yet implemented
 *
 *  void 
 *   SgGetSelectionValues(Widget                     w, 
 *			 Atom                       selection, 
 *			 Atom                       *targets, 
 *			 int                        count,
 *			 XtSelectionCallbackProc    callback,
 *			 XtPointer                  *client_data,
 *			 Time                       time );
 */

/* Modeled after XtConvert */
  void
    SgConvert(Widget       w,	                      /* may not need Widget */
	      String       from_type, 
	      XrmValuePtr  from,
	      String       to_type, 
	      XrmValuePtr  to_return );
  
/*
 * Data allocated for the conversion, returned in to_return must be XtFree'd by the user
 * The library XtMalloc's and XtReallocs this data, so delete should NOT be used!.
 */
  
  
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _SGSELECT_H_ */




