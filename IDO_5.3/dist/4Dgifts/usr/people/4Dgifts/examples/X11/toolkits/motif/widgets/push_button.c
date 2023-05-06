/*
 *    push_button.c
 *
 *     This file demonstrates creating a XmPushButton widget. It again uses
 *  the concept of the XmString [see label.c] and introduces the concept of
 *  a callback.
 *
 *                                                - Dave Shreiner
 *                                                  19 March 1991
 */

#include <Xm/Xm.h>      /* X/Motif header file */
#include <Xm/PushB.h>   /* header file for pushbutton widget type. */

main(argc, argv)
int argc;
char *argv[];
{

/*----------------------------------------------------------------------------
      Declare variables and initialize the application.  [see label.c]
-----------------------------------------------------------------------------*/

   Widget      toplevel, button;
   Arg         arg;
   XmString    xmstr;
   void        callback_handler();

   toplevel = XtInitialize(argv[0], "MotifDemo", NULL, 0, &argc, argv);

/*----------------------------------------------------------------------------
      Set up the label string for the push button.
-----------------------------------------------------------------------------*/

   xmstr = XmStringCreateSimple("Push Me");
   XtSetArg(arg, XmNlabelString, xmstr);

/*----------------------------------------------------------------------------
      Create a push button widget using the XmCreatePushButton() call.  This
      function has identical parameters to the XmCreateLabel() call in label.c.
-----------------------------------------------------------------------------*/

   button = XmCreatePushButton(toplevel, "button", &arg, 1);

/*----------------------------------------------------------------------------
      Add a callback processing function to the push button widget.  A callback
      is one of the ways that X lets a user know that something has happened.
      Each diffrent type of widget has a set of callback resources that it
      can "register" for.  Callbacks are usually like "the user activated a
      push button" or "the user moved a scrollbar", as compared to more low
      level events like "the mosue pointer was moved", or "a keyboard button
      was pressed".

      The parameters for this function are as follows :

         1) the WIDGET that the callback is to be added for (in this case,
            "button")

         2) the CALLBACK action that is of interest (the user pressing the
            button, which Motif knows as "XmNactivateCallback".  A complete
            listing of all callbacks to a particular type of widget is
            available in the OSF/Motif Programmer's reference)

         3) the FUNCTION that will respond to the callback (when the user
            does the action, execute these lines of code.  For us, the
             function is named "callback_handler" [see below])

         4) any data that we want passed to the function.  This can be a
            single value (int, float, double, etc.), or a pointer to whatever
            (structure, file, character array, etc.).  (here, we don't want to
            pass anything in, so we give a NULL value)

-----------------------------------------------------------------------------*/

   XtAddCallback(button, XmNactivateCallback, callback_handler, NULL);

/*----------------------------------------------------------------------------
      Manage the child widget (in this case "button"), let X know we want
      the window to appear, and then we'll process all the events sent from
      the X server.
-----------------------------------------------------------------------------*/

   XtManageChild(button);

   XtRealizeWidget(toplevel);
   XtMainLoop();
}



/*----------------------------------------------------------------------------
      Define a function to be passed into the XtAddCallback() call.  The
      declaration of a callback handling function is always of the from

         void function_name(widget, client_supplied_data, callback_data)
         Widget   widget;
         caddr_t  client_supplied_data, callback_data;

         the parameters are as follows :

            1) the WIDGET that the callback originated from.  The value
               returned will only every be one of the widgets that this
               function was registered with.  (in out example, the widget
               will be button widget, since that's the only widget that
               a callback was added to)

            2) the DATA that the user could supply in the last arguement to
               the XtAddCallback() call.  The caddr_t type is just a place
               holder, and should be replaced with the correct value for the
               type of data that is being passed in.  (For example, if we
               were passing in a character array (char *), then we would
               declare the "client_supplied_data" to be of type "char *", not
               caddr_t.)  [see scroll_bar.c for an example of passing data
               into a callback handling routine]

            3) information about the CALLBACK itself.  Generally, Motif defines
               a callback structure that is specific to each widget.  (For
               example, the type of structure returned from a push button
               callback is an "XmPushButtonCallbackStruct".) Once again, the
               caddr_t declaration for the argument is done for place holding.
               This is done since one callback handling routine could be used
               to process many different types of callbacks, each with a
               different type.  [see scroll_bar.c for more detail about
               using the CALLBACK parameter]

-----------------------------------------------------------------------------*/

void callback_handler(widget, client_data, callback_data)
Widget widget;
caddr_t client_data, callback_data;
{

/*----------------------------------------------------------------------------
      Do the actions that are to occur when the user presses the push button.
      In general, there would be some code here to react to some user request.
-----------------------------------------------------------------------------*/

   printf("  You pressed a push button.\n");
}
