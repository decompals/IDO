/*
 *    scroll_bar.c
 *
 *     This file illustrates a XmScrollBar widget.  In addition, it expands
 *  on callbacks by registering more than one callback to the widget, and
 *  using the client_supplied_data [see push_button.c] parameter to pass
 *  data to the callback handling routine.
 *
 *                                                 - Dave Shreiner
 *                                                   20 March 1991
 */

#include <Xm/Xm.h>         /* X/Motif header file */
#include <Xm/ScrollBar.h>  /* header file for scroll bar widget type. */


main(argc, argv)
int argc;
char *argv[];
{

/*----------------------------------------------------------------------------
      Declare variables and initialize the application.  [see label.c for more]
-----------------------------------------------------------------------------*/

   Widget   toplevel, scroll_bar;
   void     scroll();

   static char *id[] = {
      "incremented",
      "drug",
      "decremented",
      "page up'ed",
      "page down'ed",
      "end'ed",
      "home'ed"
   };

   toplevel = XtInitialize(argv[0], "MotifDemo", NULL, 0, &argc, argv);

/*----------------------------------------------------------------------------
      Create a scroll bar widget with the call XmCreateScrollBar.  The
      parameters to this call are identical to XmCreateLabel().  [see label.c]
-----------------------------------------------------------------------------*/

   scroll_bar = XmCreateScrollBar(toplevel, "scrollbar", NULL, 0);

/*----------------------------------------------------------------------------
      Register several callbacks with the scroll bar widget.  All of these
      callbacks will call the same callback handling function (scroll),
      although each will provide the function with different data.  Each
      one of these callbacks are explained in the OSF/Motif Programmer's
      Reference.

      Something new in these XtAddCallback() calls as compared to the one in
      "push_button.c" is that now we are passing data to the callback handling
      routine [see push_button.c} when it reacts to receiving a callback.  The
      last argument will pass either a single vaule (int, float, etc.) or a
      pointer.  In this instance, we are passing the address of a character
      array to be used in the callback handling function.
-----------------------------------------------------------------------------*/

   XtAddCallback(scroll_bar, XmNdecrementCallback, scroll, id[0]);
   XtAddCallback(scroll_bar, XmNdragCallback, scroll, id[1]);
   XtAddCallback(scroll_bar, XmNincrementCallback, scroll, id[2]);
   XtAddCallback(scroll_bar, XmNpageDecrementCallback, scroll, id[3]);
   XtAddCallback(scroll_bar, XmNpageIncrementCallback, scroll, id[4]);
   XtAddCallback(scroll_bar, XmNtoBottomCallback, scroll, id[5]);
   XtAddCallback(scroll_bar, XmNtoTopCallback, scroll, id[6]);


/*----------------------------------------------------------------------------
      Manage the child widget (in this case "scroll_bar"), let X know we want
      the window to appear, and then we'll process all the events sent from
      the X server.
-----------------------------------------------------------------------------*/

   XtManageChild(scroll_bar);

   XtRealizeWidget(toplevel);
   XtMainLoop();
}

/*----------------------------------------------------------------------------
      Define a callback handline routine.  [see push_button.c for a full
      explanation]  Here we are going to make use of both the client_data,
      and call_data parameters that are passed.  In this case, the client_data
      will hold the address of the character string that we passed in above,
      and the call_data will give us information about the callback
      (specifically, the new value for the location of the slider).

      Once again, we declare client_data and call_data as type caddr_t.
      This forces us to cast the value returned into the data type that we
      were expecting.  For example, in the code, we transform the client_data
      into a "char *" by casting.  In actual applications, it may be convenient
      to define the data types in the function defintion.  For example, we
      could have defined "scroll" to be as follows :

         void scroll(widget, char_string, callback_info)
         Widget                     widget;
         char                       *char_string;
         XmScrollBarCallbackStruct  callback_info;
         {
            printf("You %s the slider to value %d\n", char_string,
               callback_info);
         }

      The defintion for all available callback structures can be found in Xm.h.
      In that header file, we find XmScrollBarCallbackStruct to be defined as :

         typedef struct
         {
            int reason;
            XEvent * event;
            int value;       <---  contains the new slider location value.
            int pixel;
         } XmScrollBarCallbackStruct;

      We are interested in the "value" field, which tells up the value of the
      slider in the scrollbar.  For a complete definition of all the fields to
      any callbacks, see the OSF/Motif Programmer's Reference.
-----------------------------------------------------------------------------*/

void scroll(widget, client_data, call_data)
Widget   widget;
caddr_t  client_data, call_data;
{
   char *direction;
   XmScrollBarCallbackStruct  *callback;

   direction = (char *) client_data;
    callback = (XmScrollBarCallbackStruct *) call_data;

   printf("  You %s the slider to value %d\n", direction, callback->value);
}
