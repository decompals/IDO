#ifndef _LIBXs_h
#define _LIBXs_h

#include <Xm/Xm.h>

#ifdef JUST_LIKE_BOOK
#define XtRFloat "Float"
#else
/*
 * This has been omitted, because in X11R5 it is defined in <X11/StringDefs.h>.
 * Defining it here causes a multiple definition warning.
 */
#endif

typedef struct _menu_struct{
   char*                  name;           /* name of the button */
   void                  (*func)();       /* Callback to be invoked */
   caddr_t                data;           /* Data for the callback */
   struct _menu_struct   *sub_menu;       /* data for submenu of this button */
   int                    n_sub_items;    /* How many items in sub_menu */
   char                  *sub_menu_title; /* Title of submenu */
 } xs_menu_struct;

extern Widget    xs_create_quit_button ();
extern void      xs_invert_widget ();
extern XmString  xs_concat_words ();
extern void      xs_cvt_str_to_float ();
XmString         xs_str_array_to_xmstr();
extern Widget    xs_create_pixmap_button ();
extern Widget    xs_create_pixmap_browser ();
extern char     *xs_get_string_from_xmstring ();
void             xs_help_callback();
extern GC        xs_create_xor_gc();
/*
 * Horrible hack for BSD systems which don't have vsprintf().
 * Instead of xs_wprintf(), use the macro which matches the 
 * number of args passed. Extend as needed.
 */
#define XS_WPRINTF_ONE(_widget,_format,_arg) { \
        Arg _tmp[10];  char _tmp_buf[1000]; \
        sprintf(_tmp_buf, _format, _arg); \
        XtSetArg(_tmp[0], XmNlabelString, \
		 XmStringLtoRCreate(_tmp_buf, XmSTRING_DEFAULT_CHARSET)); \
        XtSetValues(_widget,_tmp, 1); \
	}

#define XS_WPRINTF_TWO(_widget,_format,_a,_b) { \
        Arg _tmp[10];  char _tmp_buf[1000]; \
        sprintf(_tmp_buf, _format, _a, _b); \
        XtSetArg(_tmp[0], XmNlabelString, \
		 XmStringLtoRCreate(_tmp_buf, XmSTRING_DEFAULT_CHARSET)); \
        XtSetValues(_widget,_tmp, 1); \
	}

#define XS_WPRINTF_THREE(_widget,_format,_a,_b,_c) { \
        Arg _tmp[10];  char _tmp_buf[1000]; \
        sprintf(_tmp_buf, _format, _a, _b, _c); \
        XtSetArg(_tmp[0], XmNlabelString, \
		 XmStringLtoRCreate(_tmp_buf, XmSTRING_DEFAULT_CHARSET)); \
        XtSetValues(_widget,_tmp, 1); \
	}

#define XS_WPRINTF_FOUR(_widget,_format,_a,_b,_c,_d) { \
        Arg _tmp[10];  char _tmp_buf[1000]; \
        sprintf(_tmp_buf, _format, _a, _b, _c, _d); \
        XtSetArg(_tmp[0], XmNlabelString, \
		 XmStringLtoRCreate(_tmp_buf, XmSTRING_DEFAULT_CHARSET)); \
        XtSetValues(_widget,_tmp, 1); \
	}
      
#endif


