#include <Sgt/SgtSelect.h>
#include <stdlib.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

/* Next 2 are for Debugging  */
#include <sys/types.h>
#include <unistd.h>

#include <X11/Intrinsic.h>
#include <X11/Xmu/StdSel.h>
#include <X11/Xmu/Atoms.h>
#include <X11/Xatom.h>
#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>
#include <stdio.h>

Atom A, B, C, D, E;
Atom * current;

static Boolean abConv( Display * d, XrmValue * args, Cardinal * nArgs,
		       XrmValue * source_data,
		       XrmValue * sink_data)
     
{
  if (source_data->size > 0 && source_data->addr != NULL) {
    String str = XtMalloc(source_data->size);
    int i, sourceLen = strlen( (char *)source_data->addr );
    
    printf("abConv\n");
    memcpy(str, (void *)source_data->addr, source_data->size);
    for (i = 0; i < sourceLen; i+=2)
      str[ i ] = 'B';
    sink_data->addr = (XPointer)str;
    sink_data->size = source_data->size ;
    return True;
  } else
    return False;
}


static Boolean
myConvertSelection(Widget w,
		   Atom *selection,
		   Atom *target,
		   Atom *type,
		   XtPointer *value,
		   unsigned long *length,
		   int *format)
{
  if (*target == *current) {
    int i;
    char * data;
    String name = strdup(XmuGetAtomName( XtDisplay( w ), *current));
    *type = *target;
    *length = 20;
    data = XtMalloc( (unsigned int)*length * sizeof (char));
    for (i = 0; i < *length - 1; i++) {
      data[ i ] = name[0];
    }
    data[ *length - 1 ] = '\0';
    free( name );
    *value = (XtPointer) data;
    *format = 8;
    return True;
  }
  if ( *target == XA_TARGETS(XtDisplay(w)) ) {
    Atom * stdTargets;
    unsigned long stdLen;
    Atom * targetP;
    XmuConvertStandardSelection(w,
				XtLastTimestampProcessed( XtDisplay( w )),
				selection,
				target,
				type,
				(caddr_t *)&stdTargets,
				&stdLen,
				format);
    *value = (Atom *) XtMalloc((unsigned int)(stdLen + 2) * sizeof(Atom));
    targetP = *(Atom **)value;
    *targetP++ = XInternAtom(XtDisplay(w), "MULTIPLE", False);
    *targetP++ = *current;
    *length = stdLen + (targetP - (*(Atom **)value));
    memcpy(targetP, stdTargets, sizeof(Atom) * (unsigned int)stdLen);
    XtFree((char *)stdTargets);
    *type = XA_ATOM;
    return True;
  }
  if (XmuConvertStandardSelection(w,
				  XtLastTimestampProcessed( XtDisplay( w )),
				  selection,
				  target,
				  type,
				  (caddr_t *)value,
				  length,
				  format)) {
    return True;
  }
  
  return False;
  
}


static void
mySelectionCallback(Widget w,
		    XtPointer bla,
		    Atom * bla1,
		    Atom *type,
		    XtPointer value,
		    unsigned long *length,
		    int * bla2)
{
  if ((*length != 0) && (value != NULL)) {
    printf("%s type length is: %d\n", XmuGetAtomName(XtDisplay( w ), *type), (int)*length);
    printf("Data is: %s\n", (char *)value);
    XtFree((char *)value);
    fflush(stdout);
  }
}


static void
own( Widget w, XtPointer clientData, XtPointer cbd)
{
  
  XtOwnSelection( w,
		  XA_PRIMARY,
		  XtLastTimestampProcessed( XtDisplay( w )),
		  myConvertSelection,
		  NULL,
		  NULL);
  
}

static void
get( Widget w, XtPointer clientData, XtPointer cbd)
{
  
  XtGetSelectionValue( w,
		       XA_PRIMARY,
		       *current,
		       mySelectionCallback,
		       NULL,
		       XtLastTimestampProcessed( XtDisplay( w )));
  
}

static void
typeChange( Widget w, XtPointer clientData, XtPointer cbs) {
  XmToggleButtonCallbackStruct * cbd = (XmToggleButtonCallbackStruct *)cbs;
  int which = (int)clientData;
  
  if (cbd->set) {
    switch (which) {
    case 0:
      current = &A; break;
    case 1:
      current = &B; break;
    case 2:
      current = &C; break;
    case 3:
      current = &D; break;
    case 4:
      current = &E;  break;
    default:
      break;
    }
  }
}

main (int argc, char ** argv) {
  
  XtAppContext app;
  Display * d;
  
  Widget toplevel = XtVaAppInitialize( &app, "cSelect", NULL, 0, &argc, argv, NULL, NULL);
  
  Widget form  = XtVaCreateManagedWidget("form",
					 xmFormWidgetClass, toplevel,
					 NULL);
  Widget cutB = XtVaCreateManagedWidget("cut",
					xmPushButtonWidgetClass, form,
					XmNleftAttachment, XmATTACH_FORM,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					NULL);
  Widget pasteB = XtVaCreateManagedWidget("paste",
					  xmPushButtonWidgetClass, form,
					  XmNleftAttachment, XmATTACH_WIDGET,
					  XmNleftWidget, cutB,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNbottomAttachment, XmATTACH_FORM,
					  NULL);
  
  XmString b1 = XmStringCreateLocalized("A");
  XmString b2 = XmStringCreateLocalized("B");
  Widget currentType = XmVaCreateSimpleRadioBox( form,
						 "currentType",
						 0,
						 typeChange,
						 XmVaRADIOBUTTON, b1, NULL, NULL, NULL,
						 XmVaRADIOBUTTON, b2, NULL, NULL, NULL,
						 NULL);
  XmStringFree( b1 );
  XmStringFree( b2 );
  
  XtVaSetValues( currentType,
		 XmNleftAttachment, XmATTACH_WIDGET,
		 XmNleftWidget, pasteB,
		 XmNtopAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
		 NULL);
  
  XtManageChild( currentType );
  d = XtDisplay( form );
  
  A = XInternAtom( d, "A", False);
  B = XInternAtom( d, "B", False);
  C = XInternAtom( d, "C", False);
  D = XInternAtom( d, "D", False);
  E = XInternAtom( d, "E", False);
  current = &A;
  
  
  SgtAddConverterFunc(d, "A", "B", abConv, NULL, 0);
  
  XtAddCallback( cutB, XmNactivateCallback, own, NULL); 
  XtAddCallback( pasteB, XmNactivateCallback, get, NULL);
  
  XtRealizeWidget( toplevel );
  XtAppMainLoop( app );
  
}





