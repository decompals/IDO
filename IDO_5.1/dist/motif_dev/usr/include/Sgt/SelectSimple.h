#ifndef _SelectSimple_h
#define _SelectSimple_h


#ifndef _NO_PROTO
#if    !(defined(__STDC__) && __STDC__) \
    && !defined(__cplusplus) && !defined(c_plusplus) \
    && !defined(FUNCPROTO) && !defined(XTFUNCPROTO) && !defined(XMFUNCPROTO)
#define _NO_PROTO
#endif /* __STDC__ */
#endif /* _NO_PROTO */


#include <X11/Intrinsic.h>
#include <X11/Xatom.h>


#ifdef __cplusplus
extern "C" {
#endif


/********     Public Declarations     ********/

#ifdef _NO_PROTO
	Extern Boolean	SgGetSimpleCharacterPosition () ;
	extern Boolean	SgGetSimpleBackground () ;
	extern Boolean	SgGetSimpleBitmap () ;
	extern Boolean	SgGetSimpleClass () ;
	extern Boolean	SgGetSimpleClientWindow () ;
	extern Boolean	SgGetSimpleColumnNumber () ;
	extern Boolean	SgGetSimpleCompoundText () ;
	extern Boolean	SgGetSimpleDelete () ;
	extern Boolean	SgGetSimpleDrawable () ;
	extern Boolean	SgGetSimpleFileName () ;
	extern Boolean	SgGetSimpleForeground () ;
	extern Boolean	SgGetSimpleHostname () ;
	extern Boolean	SgGetSimpleIPAddress () ;
	extern Boolean	SgGetSimpleLength () ;
	extern Boolean	SgGetSimpleLineNumber () ;
	extern Boolean	SgGetSimpleListLength () ;
	extern Boolean	SgGetSimpleModule () ;
	extern Boolean	SgGetSimpleName () ;
	extern Boolean	SgGetSimpleODIF () ;
	extern Boolean	SgGetSimpleOwnerOs () ;
	extern Boolean	SgGetSimplePixmap () ;
	extern Boolean	SgGetSimpleProcedure () ;
	extern Boolean	SgGetSimpleProcessInteger () ;
	extern Boolean	SgGetSimpleProcessString () ;
	extern Boolean	SgGetSimpleString () ;
	extern Boolean	SgGetSimpleTargets () ;
	extern Boolean	SgGetSimpleTaskInteger () ;
	extern Boolean	SgGetSimpleTaskString () ;
	extern Boolean	SgGetSimpleText () ;
	extern Boolean	SgGetSimpleTimestamp () ;
	extern Boolean	SgGetSimpleUser () ;
	extern Boolean	SgGetSimpleUser () ;

	extern Boolean	SgPostSimpleText () ;
	extern void	SgSetLoseSelectionCallback () ;
	extern void	SgSetSimpleSelectDebug();
	extern void	SgSetSimpleSelectLibDebug();
	extern void	SgSwitchBackground();
	extern Boolean	SgUnpostSimple();
#else
	extern void SgSetLoseSelectionCallback(
		Widget               w,
		Atom                 selection, 
		XtLoseSelectionProc  proc);

	extern Boolean SgGetSimpleBackground(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleBitmap(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleCharacterPosition(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleClass(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleClientWindow(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleColormap(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleColumnNumber(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleCompoundText(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleDelete(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleDrawable(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleFileName(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleForeground(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleHostname(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleIPAddress(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleLength(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleLineNumber(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleListLength(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleModule(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleName(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleODIF(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleOwnerOs(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimplePixmap(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleProcedure(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleProcessInteger(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleProcessString(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleString(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleTargets(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleTaskInteger(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleTaskString(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleText(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleTimestamp(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	extern Boolean SgGetSimpleUser(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *length, 
		XtPointer           *data);

	void SgSetSimpleSelectDebug (int level);

	void SgSetSimpleSelectLibDebug(int level);

	void SgSwitchBackground(Widget w);

	extern Boolean SgPostSimpleText(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		void                *data);

	Boolean SgUnpostSimple(
        	Widget               w,
        	Atom                 selection);


#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/



#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif



#endif /* _SelectSimple_h */
 /* DON'T ADD STUFF AFTER THIS #endif */
