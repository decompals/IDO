#ifndef _SGTSELECT_H_
#define _SGTSELECT_H_

/*
 * File:       SgtSelect.h
 *
 * This file defines the public C language API for SGI-enhanced selection and for 
 * selection data type converters.
 *
 */

#include <Xm/Xm.h>
#include <X11/Xresource.h>

/* The following macro defines the name of the procedure which dso's of
 * converters should provide to register all of the converters in a dso.
 */
#define REGISTER_DSO_INIT_PROC "registerInitProc"

/*
 *  Resource strings for enabling enhanced selection
 */
#ifndef SgCEnhanceSelection
#define SgCEnhanceSelection "EnhanceSelection"
#endif

#ifndef SgNenhanceSelection
#define SgNenhanceSelection "enhanceSelection"
#endif

#ifdef __cplusplus
extern "C" {
#endif 
  
  
typedef void (*SgtRegisterDsoProc)( Display * );   /* display */
  
typedef Boolean (*SgtTypeConverter)(Display  *,    /* display */
				    XrmValue *,    /* args   */
				    Cardinal *,    /* num args */
				    XrmValue *,    /* from */
				    XrmValue *);   /* to */
  
  
/* Modeled after XtAppAddConverter and XtSetTypeConverter */
  void 
    SgtAddConverterFunc(Display            * d,
			String             from_type, 
			String             to_type,
			SgtTypeConverter   converter,
			XtConvertArgList   convert_args,  /* currently not used */
			Cardinal           num_args);     /* currently not used */
  
  
/* 
 * SgAddConverterProg
 *
 * Register new converter program.
 * A routine unique to the Sg selection API, not based an existing Xt function.
 */ 
  void 
    SgtAddConverterProg(Display * d,
			String  from_type, 
			String  to_type,
			char    **argv,
			String  converter_prog );
  
  
  
/* 
 * SgtAddConverterDso
 *
 * Register new converter Dso.
 * A routine unique to the Sg selection API, not based an existing Xt function.
 */ 
  void 
    SgtAddConverterDso(Display * d,
		       String  dso_name );
  
  
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _SGTSELECT_H_ */
