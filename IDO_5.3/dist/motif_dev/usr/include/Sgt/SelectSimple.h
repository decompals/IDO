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

    /* data type independent entry points */
	extern void	SgtSetLoseSelectionCallback () ;
	extern Boolean	SgtGetSimple();
	extern Boolean	SgtPostSimple();
	extern Boolean	SgtUnpostSimple();

    /* per data type entry points */
	extern Boolean	SgtGetSimpleBACKGROUND () ;
	extern Boolean	SgtGetSimpleBITMAP () ;
	extern Boolean	SgtGetSimpleCHARACTER_POSITION () ;
	extern Boolean	SgtGetSimpleCLASS () ;
	extern Boolean	SgtGetSimpleCLIENT_WINDOW () ;
	extern Boolean	SgtGetSimpleCOLORMAP () ;
	extern Boolean	SgtGetSimpleCOLUMN_NUMBER () ;
	extern Boolean	SgtGetSimpleCOMPOUND_TEXT () ;
	extern Boolean	SgtGetSimpleDELETE () ;
	extern Boolean	SgtGetSimpleDRAWABLE () ;
	extern Boolean	SgtGetSimpleFILE_NAME () ;
	extern Boolean	SgtGetSimpleFOREGROUND () ;
	extern Boolean	SgtGetSimpleHOSTNAME () ;
	extern Boolean	SgtGetSimpleIP_ADDRESS () ;
	extern Boolean	SgtGetSimpleLENGTH () ;
	extern Boolean	SgtGetSimpleLINE_NUMBER () ;
	extern Boolean	SgtGetSimpleLIST_LENGTH () ;
	extern Boolean	SgtGetSimpleMODULE () ;
	extern Boolean	SgtGetSimpleNAME () ;
	extern Boolean	SgtGetSimpleODIF () ;
	extern Boolean	SgtGetSimpleOWNER_OS () ;
	extern Boolean	SgtGetSimplePIXMAP () ;
	extern Boolean	SgtGetSimplePROCEDURE () ;
	extern Boolean	SgtGetSimplePROCESS_INTEGER () ;
	extern Boolean	SgtGetSimplePROCESS_STRING () ;
	extern Boolean	SgtGetSimpleSTRING () ;
	extern Boolean	SgtGetSimpleTARGETS () ;
	extern Boolean	SgtGetSimpleTASK_INTEGER () ;
	extern Boolean	SgtGetSimpleTASK_STRING () ;
	extern Boolean	SgtGetSimpleTEXT () ;
	extern Boolean	SgtGetSimpleTIMESTAMP () ;
	extern Boolean	SgtGetSimpleUSER () ;

	extern Boolean	SgtPostSimpleBACKGROUND () ;
	extern Boolean	SgtPostSimpleBITMAP () ;
	extern Boolean	SgtPostSimpleCHARACTER_POSITION () ;
	extern Boolean	SgtPostSimpleCLASS () ;
	extern Boolean	SgtPostSimpleCLIENT_WINDOW () ;
	extern Boolean	SgtPostSimpleCOLORMAP () ;
	extern Boolean	SgtPostSimpleCOLUMN_NUMBER () ;
	extern Boolean	SgtPostSimpleCOMPOUND_TEXT () ;
	extern Boolean	SgtPostSimpleDELETE () ;
	extern Boolean	SgtPostSimpleDRAWABLE () ;
	extern Boolean	SgtPostSimpleFILE_NAME () ;
	extern Boolean	SgtPostSimpleFOREGROUND () ;
	extern Boolean	SgtPostSimpleHOSTNAME () ;
	extern Boolean	SgtPostSimpleIP_ADDRESS () ;
	extern Boolean	SgtPostSimpleLENGTH () ;
	extern Boolean	SgtPostSimpleLINE_NUMBER () ;
	extern Boolean	SgtPostSimpleLIST_LENGTH () ;
	extern Boolean	SgtPostSimpleMODULE () ;
	extern Boolean	SgtPostSimpleNAME () ;
	extern Boolean	SgtPostSimpleODIF () ;
	extern Boolean	SgtPostSimpleOWNER_OS () ;
	extern Boolean	SgtPostSimplePIXMAP () ;
	extern Boolean	SgtPostSimplePROCEDURE () ;
	extern Boolean	SgtPostSimplePROCESS_INTEGER () ;
	extern Boolean	SgtPostSimplePROCESS_STRING () ;
	extern Boolean	SgtPostSimpleSTRING () ;
	extern Boolean	SgtPostSimpleTARGETS () ;
	extern Boolean	SgtPostSimpleTASK_INTEGER () ;
	extern Boolean	SgtPostSimpleTASK_STRING () ;
	extern Boolean	SgtPostSimpleTEXT () ;
	extern Boolean	SgtPostSimpleTIMESTAMP () ;
	extern Boolean	SgtPostSimpleUSER () ;

    /* unsupported entry points */
	extern void	SgtSetSimpleSelectDebug();
	extern void	SgtSetSimpleSelectLibDebug();
	extern void	SgtSwitchBackground();
#else
    /* data type independent entry points */
	extern void SgtSetLoseSelectionCallback(
		Widget               w,
		Atom                 selection, 
		XtLoseSelectionProc  proc);

	extern Boolean SgtGetSimple(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP,
		Atom		     target,
		Atom		     type);

	extern Boolean SgtPostSimple(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data,
		Atom		     target,
		Atom		     type,
		int		     format);

	Boolean SgtUnpostSimple(
        	Widget               w,
        	Atom                 selection);

    /* per data type entry points */
	extern Boolean SgtGetSimpleBackground(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleBitmap(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleCharacterPosition(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleClass(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleClientWindow(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleColormap(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleColumnNumber(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleCompoundText(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleDelete(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleDrawable(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleFileName(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleForeground(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleHostname(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleIPAddress(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleLength(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleLineNumber(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleListLength(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleModule(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleName(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleODIF(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleOwnerOs(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimplePixmap(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleProcedure(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleProcessInteger(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleProcessString(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleString(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleTargets(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleTaskInteger(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleTaskString(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleText(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleTimestamp(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);

	extern Boolean SgtGetSimpleUser(
		Widget               w, 
		Atom                 selection, 
		unsigned long       *lengthP, 
		XtPointer           *dataP);


	extern Boolean SgtPostSimpleBackground(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleBitmap(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleCharacterPosition(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleClass(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleClientWindow(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleColormap(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleColumnNumber(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleCompoundText(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleDelete(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleDrawable(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleFileName(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleForeground(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleHostname(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleIPAddress(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleLength(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleLineNumber(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleListLength(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleModule(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleName(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleODIF(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleOwnerOs(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimplePixmap(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleProcedure(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleProcessInteger(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleProcessString(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleString(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleTargets(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleTaskInteger(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleTaskString(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleText(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleTimestamp(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

	extern Boolean SgtPostSimpleUser(
		Widget               w, 
		Atom                 selection, 
		unsigned long        length, 
		XtPointer            data);

    /* unsupported entry points */
	void SgtSetSimpleSelectDebug (int level);

	void SgtSetSimpleSelectLibDebug(int level);

	void SgtSwitchBackground(Widget w);


#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/



#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif



#endif /* _SelectSimple_h */
 /* DON'T ADD STUFF AFTER THIS #endif */
