/*
 *   start.c
 *
 *       A basic framework for a Motif type program which provides a 
 *   simple starting point for a motif program.  Included is the bare 
 *   minimum required for a working program.  
 *   NOTE: this program will not function without adding some widget as
 *         a child of the toplevel, or setting some resources to make 
 *         both the height and width of the toplevel widget be non-zero.
 */
#include <Xm/Xm.h>

main(argc, argv)
int argc;
char *argv[];
{
   Widget toplevel;

   toplevel = XtInitialize(argv[0], "MotifDemo", NULL, 0, &argc, argv);

   XtRealizeWidget(toplevel);
   XtMainLoop();
}
