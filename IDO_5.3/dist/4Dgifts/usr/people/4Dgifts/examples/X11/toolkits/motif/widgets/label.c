/*
 *    label.c
 *
 *      This file illustrates a simple "Hello World" type program in Motif
 *  using a label widget.  It also introduces the concept of Motif compound
 *  strings.  In an attempt to be widely useful, Motif does not display
 *  character strings of type char *.  It uses a defined type of XmString,
 *  an supplies functions for creating XmStrings, and extracting actual
 *  ASCII character strings from the XmStrings.  In addition, it also
 *  illustrates setting widget resources from within a program.
 *
 *                                              - Dave Shreiner
 *                                                18 March 1991
 */

#include <Xm/Xm.h>      /* X/Motif header file ... always required for Motif */
                        /* Also, Xm/Xm.h must preceed all other Xm/*.h files */
#include <Xm/Label.h>   /* header file for Label widget type. */

main(argc, argv)
int argc;
char *argv[];
{

/*----------------------------------------------------------------------------
      Declare variables - All of the following variables are types specific
      to X Intrinsic toolkits of X/Motif.

      Widget   - principle building block of all X toolkit applications.
      XmString - Motif's character string.  Used in almost all applications
         where displaying text is desired.
      Arg      - X toolkits' type for inquiring/changing a characterstic of
         a widget.
-----------------------------------------------------------------------------*/


   Widget   toplevel, label;
   XmString xmstr;
   Arg      arg[1];


/*----------------------------------------------------------------------------
      Create the main widget for the application, as well as opening a
      connection to the X server.  This line (in some form) is required in
      most X toolkit applications.  Its responsible for letting the X server
      know that an application requires a window that needs to be created,
      takes care of all communication issues between the X server and the
      client (This application program), and creates the first widget.  This
      widget, is the base window for all other widgets in the application to
      reside in.

      The parameters to the function are as follows :

         1) the APPLICATION NAME as a character pinter.  X/Motif uses the word
            "Application" instead of "program".  Passing in argv[0] in here is
            usually a safe practice.

         2) the CLASS the application belongs to as a character string.  For
            instance, all programs in the ~4Dgifts/motif directory are of the
            class "MotifDemo".  This is used for advanced situations where the
            programmer wants all the applications in a class to look the same.

         3) A POINTER to structures that define parsing for special user added
            command line parameters.  (This is sufficently advanced that it in
            all applications in the ~4Dgifts/motif directory, this argument
             will be set to NULL)

         4) The NUMBER of structures that are passed in the previous arguement.
            Since we are setting argument 3 to NULL, we pass a zero (0) here.

         5) A POINTER to the number of command line arguments passed to the
            program.  Essientally, you can always set this to "&argc" if argc
            is the name defined in the main() definition.  This parameter is
            destructively changed as X parses any command line options that it
            understands, leaving the resulting list for the user to proess.

         6) A POINTER to the character arrays which contain the command line
            arguments.  This usually be set to "argv" if argv is the name
            defined in the main() definition.  This list is destructively
            changed in the same manner as the previous argument.
-----------------------------------------------------------------------------*/


   toplevel = XtInitialize(argv[0], "MotifDemo", NULL, 0, &argc, argv);


/*-----------------------------------------------------------------------------
   Create a Motif type character string for use as the text in the label
      widget (yet to be created).  The function "XmStringCreateSimple" takes
      as a parameter the character string that is to be converted to a Motif
      type string, and returns the new XmString that was created.
-----------------------------------------------------------------------------*/


   xmstr = XmStringCreateSimple("Hello World");


/*-----------------------------------------------------------------------------
      Set up the Arg structure with the function "XtSetArg".  This function
      prepares your request in a form that is processable by Motif.  In this
      case, we want to set the label string (XmNlabelString)  to be the
      XmString that we just created.  A short note : every type of widget has
      a list of attributes in the from XmN<some attribute>.  Every XmN* type
      resource requires a specific type of arguement in the XtSetArg call.
      A complete list of resources and their required types are available in
      various references, including the OSF/Motif Programmer's Reference Manual.

       The parameters for this functions are as follows :

         1) The Arg structure that is to be set.  In this case, arg[0].

         2) The NAME of the attribute (also called resources) that is
            understood by X/Motif.  In this case, we setting the label string
            of a widget, so the name used is "XmNlabelString".  A complete list
            can be found in Xm.h, but a better reference is the OSF/Motif
            Programmer's reference.  It cross-references which attributes are
            available to a particular widget.

         3) The type of value that is expected with the resouce in argument 2.
            This, one again, is detailed in the OSF/Motif Programmer's
            reference.  In this case, the XmNlabelString expects an XmString
            type variable, of which "xmstr" is.
-----------------------------------------------------------------------------*/


   XtSetArg(arg[0], XmNlabelString, xmstr);


/*-----------------------------------------------------------------------------
      Finally create the label widget with the desired label string (passed in
      as the &arg parameter).

       This function takes as parameters, the following, and returns the newly
      created label widget :

         1) The WIDGET that is to be the parent of this widget.  In this case,
            we are making the "toplevel" widget be the parent of the label.

         2) The NAME of the widget as a character array.  Here we are naming
            our label widget "label".  This name is used extensively for
            setting resources outside of the program (in an .Xdefaults file,
            for example)

         3) A POINTER to the Arg strutures that were defined to change the
            resources of this widget.  Here, we pass in arg, since its an array.
            (if it were a single structure, we would pass "&arg")

         4) The NUMBER of Arg structures in the previous argument.
-----------------------------------------------------------------------------*/


   label = XmCreateLabel(toplevel, "label", arg, 1);


/*-----------------------------------------------------------------------------
      After being created, the widget needs to be managed.  This is
      accomplished with the XtManageWidget() call. The parameter for this
      function is the widget that needs to be managed.
-----------------------------------------------------------------------------*/


   XtManageChild(label);


/*-----------------------------------------------------------------------------
      Alert the X server that we're done creating out appilcation and wish it
      to be displayed on the terminal.  This function (usually) takes the
      widget that was created with the XtInitialize() call as a parameter
-----------------------------------------------------------------------------*/


   XtRealizeWidget(toplevel);


/*-----------------------------------------------------------------------------
      Enter the main XEvent handling loop for the program. X windows are
      principaly an event driven system.  They spend most of their time
      waiting for something to occur, and then react to that action.  The
      XtMainLoop() call is essientally an endless loop which process events
      as they occur.
-----------------------------------------------------------------------------*/


   XtMainLoop();
}
