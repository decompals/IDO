/*
 * $Header: /hosts/bonnie/proj/banyan/isms/buildtools/books/Swpkg_UG/RCS/main.c,v 1.10 1996/01/21 01:04:04 dvogt Exp $
 *
 * finance -- Mortgage Loan Calculator
 *
 * (c) Copyright 1993, Kirk Erickson and Silicon Graphics Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Kirk Erickson and
 * Silicon Graphics Inc. not be used in advertising or publicity pertaining
 * to distribution of the software without specific, written prior
 * permission.  Kirk Erickson and Silicon Graphics Inc. make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE ABOVE-NAMED DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE ABOVE-NAMED BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

/* $Log: main.c,v $
 * Revision 1.10  1996/01/21  01:04:04  dvogt
 * No Message Supplied
 *
 * Revision 1.7  1996/01/13  00:30:03  gloria
 * No Message Supplied
 *
 * Revision 1.6  1996/01/12  23:43:43  wanke
 * No Message Supplied
 *
 * Revision 1.5  1995/10/20  20:24:19  dvogt
 * No Message Supplied
 *
 * Revision 1.4  1995/08/16  15:56:01  clarke
 * No Message Supplied
 *
 * Revision 1.4  1993/12/01  00:21:09  kirke
 * *** empty log message ***
 *
 * Revision 1.4  1993/12/01  00:21:09  kirke
 * *** empty log message ***
 *
 * Revision 1.3  93/11/30  16:03:43  kirke
 * Changed default background to DarkGreen
 * Made starting years=15 and amount=75k.
 * 
 * Revision 1.2  93/08/07  22:30:49  kirke
 * Recomputing PRINCIPAL rather than YEARS when PAYMENNT is changed.
 * 
 * Revision 1.1  1993/06/07  16:24:54  kirke
 * Initial revision
 *
 */

#include <stdio.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h> 
#include <Xm/TextF.h> 

void paymentTextField (Widget, XtPointer, XtPointer);
void principalTextField (Widget, XtPointer, XtPointer);
void rateTextField (Widget, XtPointer, XtPointer);
void yearsTextField (Widget, XtPointer, XtPointer);

Widget  _paymentTextField;
Widget  _paymentLabel;
Widget  _principalLabel;
Widget  _principalTextField;
Widget  _rateLabel;
Widget  _rateTextField;
Widget  _rowColumn;
Widget  _yearsLabel;
Widget  _yearsTextField;

float _y;	/* years */
float _p;	/* principal */
float _i;	/* interest rate */
float _r;	/* monthly payment */
float _n;	/* payments per year */


void computePayment()
{
	char buf[100];
	float w, ka, z;

	w = _n*_y;
	ka = 1/((_i/100.00)/_n+1);
	_r = ((_i/100.00)*(_p/_n))/(1-powf(ka, w));

	sprintf(buf, "%.2f", _r);
	XmTextFieldSetString(_paymentTextField, buf);
}

/*
    Pickup initial text field values, and compute monthly payment
    (setting Payment text field)
*/
Initialize()
{
	char *str;
	
	str = XmTextFieldGetString(_yearsTextField);
	sscanf(str, "%f", &_y);
	XtFree(str);

	str = XmTextFieldGetString(_principalTextField);
	sscanf(str, "%f", &_p);
	XtFree(str);

	str = XmTextFieldGetString(_rateTextField);
	sscanf(str, "%f", &_i);
	XtFree(str);

	_n = 12;

	computePayment();
}

void computeYears()
{
	float	i, x;
	char buf[100];

	i = _i/1200.00;
	x = (float)(log10((double)(_r/(_r-i*_p)))/log10((double)(1+i)));
	sprintf(buf, "%.1f", x/12);
	XmTextFieldSetString(_yearsTextField, buf);
}

void computePrincipal()
{
	char buf[100];
	float w, ka, z;

	w = _n*_y;
	ka = 1/((_i/100.00)/_n+1);

	_p = _r*(1-powf(ka, w))/(_i/100.00)*_n;

	sprintf(buf, "%.2f", _p);
	XmTextFieldSetString(_principalTextField, buf);
}

/*
// These are default resources for widgets in objects of this class
// All resources will be prepended by *<name> at instantiation,
// where <name> is the name of the specific instance, as well as the
// name of the baseWidget. These are only defaults, and may be overriden
// in a resource file by providing a more specific resource name
*/

String _defaultResources[] = {
	"*background: DarkGreen",
	"*fontList: 12x24",
        "*yearsLabel.labelString:  Years",
        "*yearsTextField.value:  15",
        "*principalLabel.labelString:  Principal",
        "*principalTextField.value:  75000",
        "*rateLabel.labelString:  Rate",
        "*rateTextField.value:  6.75",
        "*paymentLabel.labelString:  Payment",
        "*paymentTextField.value:  663.68",
        NULL
};

void setDefaultResources(const Widget w, const String *resourceSpec)
{
	int i;
	Display *dpy = XtDisplay(w);
	XrmDatabase rdb = NULL;

	/* Create empty resource database */

	rdb = XrmGetStringDatabase("");

	/* Add component resources, prepending class name */

	i = 0;
	while (resourceSpec[i] != NULL) {
		char buf[1000];

		sprintf(buf, "%s*%s", "Finance", resourceSpec[i++]);
		XrmPutLineResource(&rdb, buf);
	}

#if (XlibSpecificationRelease>=5)
	if (rdb) {
		XrmDatabase db = XtDatabase(dpy);
		XrmCombineDatabase(rdb, &db, FALSE);
	}
#else
	if (rdb) {
		XrmMergeDatabases(dpy->db, &rdb);
		dpy->db = rdb;
	}
#endif
}


void CreateWidgets(Widget parent)
{

    _rowColumn = XtVaCreateWidget ("rowColumn",
                              xmRowColumnWidgetClass,
                              parent,
                              XmNpacking, 	XmPACK_COLUMN, 
                              XmNorientation, 	XmHORIZONTAL, 
                              XmNnumColumns, 	4, 
                              XmNpacking, 	XmPACK_COLUMN, 
                              XmNentryAlignment, XmALIGNMENT_END, 
                              NULL ) ;

    _yearsLabel = XtVaCreateManagedWidget  ( "yearsLabel",
                              xmLabelWidgetClass,
                              _rowColumn, 
                              XmNrecomputeSize,	TRUE, 
                              NULL ) ;


    _yearsTextField = XtVaCreateManagedWidget  ( "yearsTextField",
                              xmTextFieldWidgetClass,
                              _rowColumn, 
                              XmNcolumns, 	12, 
                              NULL ) ;

    XtAddCallback ( _yearsTextField,
                              XmNvalueChangedCallback,
                              yearsTextField,
                              (XtPointer) NULL ); 


    _principalLabel = XtVaCreateManagedWidget  ( "principalLabel",
                              xmLabelWidgetClass,
                              _rowColumn, 
                              XmNrecomputeSize, 	TRUE, 
                              NULL ) ;


    _principalTextField = XtVaCreateManagedWidget  ( "principalTextField",
                              xmTextFieldWidgetClass,
                              _rowColumn, 
                              XmNcolumns, 	12, 
                              NULL ) ;

    XtAddCallback ( _principalTextField,
                              XmNvalueChangedCallback,
                              principalTextField,
                              (XtPointer) NULL ); 

    _rateLabel = XtVaCreateManagedWidget  ( "rateLabel",
                              xmLabelWidgetClass,
                              _rowColumn, 
                              XmNrecomputeSize, 	TRUE, 
                              NULL ) ;


    _rateTextField = XtVaCreateManagedWidget  ( "rateTextField",
                              xmTextFieldWidgetClass,
                              _rowColumn, 
                              XmNcolumns, 	12, 
                              NULL ) ;

    XtAddCallback ( _rateTextField,
                              XmNvalueChangedCallback,
                              rateTextField,
                              (XtPointer) NULL ); 

    _paymentLabel = XtVaCreateManagedWidget  ( "paymentLabel",
                              xmLabelWidgetClass,
                              _rowColumn, 
                              XmNrecomputeSize, 	TRUE, 
                              NULL ) ;


    _paymentTextField = XtVaCreateManagedWidget  ( "paymentTextField",
                              xmTextFieldWidgetClass,
                              _rowColumn, 
                              XmNcolumns, 	12, 
                              NULL ) ;

    XtAddCallback ( _paymentTextField,
#if 0
                              XmNvalueChangedCallback,
#else
                              XmNactivateCallback,
#endif
                              paymentTextField,
                              (XtPointer) NULL ); 
}


void paymentTextField (Widget w, XtPointer clientData, XtPointer callData) 
{ 
	char *str = XmTextFieldGetString(w);
	sscanf(str, "%f", &_r);
	XtFree(str);

	computePrincipal();
}

void principalTextField (Widget w, XtPointer clientData, XtPointer callData) 
{ 
	char *str = XmTextFieldGetString(w);
	sscanf(str, "%f", &_p);
	XtFree(str);

	computePayment();
}

void rateTextField (Widget w, XtPointer clientData, XtPointer callData) 
{ 
	char *str = XmTextFieldGetString(w);
	sscanf(str, "%f", &_i);
	XtFree(str);

	computePayment();
}

void yearsTextField (Widget w, XtPointer clientData, XtPointer callData) 
{ 
	char *str = XmTextFieldGetString(w);
	sscanf(str, "%f", &_y);
	XtFree(str);

	computePayment();
}


#if (XlibSpecificationRelease>=5)
void main(int argc, char **argv)
#else
void main(unsigned int argc, char **argv)
#endif
{
	Widget		toplevel;
	XtAppContext	app;
	
	/* Initialize the Intrinsics */
	
	toplevel = XtAppInitialize(&app, "Finance", NULL, 0, 
				&argc, argv, NULL, NULL, 0);
	
	setDefaultResources(toplevel, _defaultResources);

	CreateWidgets(toplevel);

	XtManageChild(_rowColumn);

	Initialize();
	
	/* Realize all widgets and enter the main event loop */
	
	XtRealizeWidget(toplevel);
	XtAppMainLoop(app);
}
