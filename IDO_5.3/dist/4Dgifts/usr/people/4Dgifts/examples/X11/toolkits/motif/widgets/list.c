/*
 *    list.c
 *
 *     This file illustrates a simple use of a Motif list widget.  The
 *  program requires as input a simple text file that contains lines ended
 *  with newlines.  A sample file is provided, named "list.data".  This
 *  example also illustrates the use of XmStrings, setting resources, and
 *  callback routines and processing.
 *
 *                                               - Dave Shreiner
 *                                                 19 March 1991
 */

#include <stdio.h>      /* header file for handling file I/O */
#include <Xm/Xm.h>      /* X/Motif header file */
#include <Xm/List.h>    /* header file for List widget type. */

#define MAX_ENTRIES  20 /* Define a constant for number of list entries */
#define LINE_LENGTH  80 /* Define a constant for maximum line length */


main(argc, argv)
int argc;
char *argv[];
{
   int      i;
   char     list_entry[LINE_LENGTH], *ch;
   FILE     *file;
   Arg      arg[4];
   Widget   toplevel, list;
   XmString xmstr[MAX_ENTRIES];
   void     callback_handler();

   if (argc < 2) {
      printf("  usage : %s <filename>\n\n", argv[0]);
      exit();
   }

   if ((file = fopen(argv[1], "r")) == NULL) {
      printf("  unable to open file '%s'\n", argv[1]);
      exit();
   }

   for (i = 0; !feof(file) && i < MAX_ENTRIES; i++) {
      fgets(list_entry, LINE_LENGTH, file);

/*-----------------------------------------------------------------------------
      The following line replace the NEWLINE ('\n') character at the end of
      each line that was read from the file to a NULL character.  This is in
      preparation for converting the character strings to Motif XmString types.
-----------------------------------------------------------------------------*/

      list_entry[strlen(list_entry) - 1] = NULL;

/*-----------------------------------------------------------------------------
      Create a Motif type character string for use as the text in an entry of
      the list widget (yet to be created).  [see label.c]
-----------------------------------------------------------------------------*/

      xmstr[i] = XmStringCreateSimple(list_entry);
   }

/*-----------------------------------------------------------------------------
      Decrement the number of strings read as to remove the redundancy of the
      last string.
-----------------------------------------------------------------------------*/

   --i;


   toplevel = XtInitialize(argv[0], "MotifDemo", NULL, 0, &argc, argv);

/*-----------------------------------------------------------------------------
      Set up resources for the list widget, including the data that was read
      from the file.  [see label.c for setting up resouces]
-----------------------------------------------------------------------------*/

   XtSetArg(arg[0], XmNitemCount, i);
   XtSetArg(arg[1], XmNitems, xmstr);
   XtSetArg(arg[2], XmNvisibleItemCount, i);
   XtSetArg(arg[3], XmNselectionPolicy, XmSINGLE_SELECT);

/*-----------------------------------------------------------------------------
      Create the list widget with the data from the file "list.data" in it.
-----------------------------------------------------------------------------*/

   list = XmCreateList(toplevel, "list", arg, 4);

/*-----------------------------------------------------------------------------
      Add a callback for when the user selects a single item.
-----------------------------------------------------------------------------*/

   XtAddCallback(list, XmNsingleSelectionCallback, callback_handler, NULL);


   XtManageChild(list);

   XtRealizeWidget(toplevel);
   XtMainLoop();
}


/*-----------------------------------------------------------------------------
      Define a callback function to print out the user's selection to
      standard output.  Once again we use the information in the callback
      structure [see scroll_bar.c] to determine what the user did.

      In addition, we use the function XmStringGetLtoR() to retrieve the true
      character representation from the XmString type.  This call is fairly
      handy, and is used in the following manner :

         XmStringGetLtoR(xm_string, char_set, string);

      the parameters required are :

         1) the XmString that has the data in that we want.  In the example
            below, the callback structure has a field which contains an
            XmString of the item that was selected from the list widget.

         2) The CHARACTER SET that the XmString is encoded in.  In most cases,
            this can be set to "XmSTRING_OS_CHARSET".  This defines the
            translation from the XmString to normal ASCII characters.  Two
            notes here :

               a) XmSTRING_OS_CHARSET is defined in Xm.h.  Its simpliest and
                  safest to use the defined version, unless you have a specific
                  need to use another charset.

               b) The function XmStringCreateSimple() uses (unless you specify
                  otherwise ... See OSF/Motif Programmer's Reference for more
                  details) XmSTRING_OS_CHARSET to encode the character string.

                  The function for translating character strings into XmStrings
                  with user chosen character sets is

                     XmStringCreate(char *text, XmStringCharSet charset);

                  which returns an XmString.

                  For example, XmStringCreateSimple() (in label.c) could be
                  replaced with

                     XmStringCreate(text, XmSTRING_OS_CHARSET);

            3) the ADDRESS of a pointer to a character (define string as
               "char *string", and pass string to XmStringGetLtoR by using
               "&string".   See below for actual code)
-----------------------------------------------------------------------------*/

void callback_handler(widget, client_data, callback)
Widget                  widget;
caddr_t                 client_data;
XmListCallbackStruct    *callback;
{
   char *string;
   int i;

   XmStringGetLtoR(callback->item, XmSTRING_OS_CHARSET, &string);

   printf("  You chose item %d : %s\n", callback->item_position, string);
}
