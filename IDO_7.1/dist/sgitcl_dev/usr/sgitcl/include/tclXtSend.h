/*
 *      "$Revision: 1.2 $"
 *      File: tcl_xt_send.h
 */

#ifndef _TCL_XT_SEND_H
#define _TCL_XT_SEND_H

#include <X11/Intrinsic.h>
#include <tcl.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int
TclXtSend_RegisterInterp _ANSI_ARGS_ ((Tcl_Interp *interp,
    		char *name, Widget toplevel));

#ifdef __cplusplus
}
#endif

#endif /* _TCL_XT_SEND_H */
